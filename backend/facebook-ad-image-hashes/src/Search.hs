{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Search where

import           Control.Error                   (ExceptT(..), MaybeT(..),
                                                  runExceptT, noteT)
import           Control.Monad                   (when)
import           Data.Bifunctor                  (first)
import           Data.Bits
import           Data.Ord                        (comparing)
import           Data.List                       (scanl', sort, sortBy,  groupBy)
import           Data.Semigroup                  ((<>))
import qualified Data.Text                       as T
import qualified Data.Aeson                      as A
import qualified Data.Aeson.Parser               as A
import           Data.Maybe                      (listToMaybe)
import qualified Data.ByteString.Lazy            as BSL
import           Data.PHash                      (PHash(..), hammingDistance, imageHash)
import           Data.Traversable                (for)
import           Data.Word
import qualified Database.PostgreSQL.Simple      as PG
import qualified Data.KdMap.Dynamic              as KD
import qualified Network.HTTP.Client             as HTTP
import qualified Network.HTTP.Client.TLS         as HTTP
import           System.Directory                (createDirectoryIfMissing,
                                                  getTemporaryDirectory,
                                                  removePathForcibly)
import           System.FilePath                 (pathSeparator)

import           Queries

------------------------------------------------------------------------------
-- | Search Trees are KD-trees. We alias `Int` to `AxisType` in case we want
--   to explore other axis types later
newtype SearchTree = SearchTree
  { getSearchTree :: KD.KdMap AxisType PHash [URL] }
  deriving (Show)

type AxisType = Int


------------------------------------------------------------------------------
-- | Convert a PHash into a list of axis values
--   PHash is a 64-bit word, so we will break it into 8 word8's
--   This is probably not a meaningful grouping in the phash
--   space, but at least it it nevertheless gives us a
--   multidimensional space for organizing our phashes
--
--   TODO: check if such a grouping _actually_ speeds
--         up our searches
--   TODO: This is super sketchy. I don't know how the dimensions of a
--         phash relate to the bits of the hash. So I just assume that each
--         Word8 is a single dimension. It's likely that making this the
--         basis function doesn't line up with the hamming distances
--         between images, so this will break the invariants of kd-trees
--         and result in lookups that fail to find the real best targets
hashToKdSpace :: PHash -> [AxisType]
hashToKdSpace (PHash word64) = map bitRange [0..7]
  where
    -- To take the nth element of a Word64,
    --  1) Create a Word8-sized mask (255 :: Word64)
    --  2) Shift it to the right by n words
    --  3) Mask the input with it
    --  4) Shift the result to the left by n words
    bitRange n =
      let iShift = 8 * n
          mask   = 255 `shiftL` iShift
          masked = word64 .&. mask
      in  fromIntegral $ masked `shiftR` iShift


------------------------------------------------------------------------------
-- | Import a list of (hash,[url]) pairs into a search tree
--   Decoding assumes tha the list is sorted by hash
loadCache :: FilePath -> IO SearchTree
loadCache cacheFile = do
  f <- BSL.readFile cacheFile

  -- The empty kdtree defines its point-decomposition function (how to turn a
  -- point into a list of dimensions), and custom distance function
  let kdTree0 = KD.emptyWithDist
                hashToKdSpace
                (\x y -> (hammingDistance x y)^2)

  -- Decode the json as an association list of Word64 (raw phash) and [URL]
  case A.decode @[(Word64, [URL])] f of
    Nothing -> error $ "Failed to decode cache file: " ++ cacheFile

    -- On successful decode, wrap the Word64s in PHash type, import them into
    -- a kd-tree, and wrap the result as type @SearchTree@
    Just ps -> return . SearchTree $ KD.batchInsert kdTree0 (fmap (first PHash) ps)


------------------------------------------------------------------------------
-- | Fetch images and phashes out of the `ads` table, fold them into
--   search tree and cache that for use across search requests
--
--   The cache differs from the images/hash query slightly
--   The database query's type is `[(PHash, URL)]`, and the cache's type
--   is `[(PHash, [URL])]`. We choose a phash similarity threshold, and for
--   consecutive query rows that are less different than the threshold, we
--   collect the URLs under the first entry's phash.
--   When the threshold is 0, the effect is that we deduplicate identical
--   images that appear under different rows from the `ads` table
--   TODO: should we deduplicate further by holding a Set of URLs rather
--         than a list? I think so
generateCache :: PG.ConnectInfo
              -> IdentityGroupingThreshold
              -> FilePath
              -> IO ()
generateCache cfg (IdentityGroupingThreshold thr) fp = do

  misaligned <- countImageHashMisalignment cfg
  when (not $ null misaligned) $
    error $
      "Database error: Some hash array did not match image array in size: "
      ++ show misaligned

  rs <- fetchSortedPhashes cfg
  when (null rs) $
    error "No valid phashes found in database"

  -- Helper function defines how to collapse query rows with identical-enough
  -- phash into a list of URLs under a single phash
  let squashIdentityGroup xs = case xs of
        [] -> error
              "Impossible case: groupBy gave a self-similar set that's empty"
        ((hash1, url1) : rest) -> (hash1, url1: (snd <$> rest))

  -- group by phash similarity of consequitive rows (the sql query returns
  -- rows sorted by phash), and squash the group members together under a
  -- single phash
  let rs' = fmap (first (\(PHash w64) -> w64))
            $ fmap squashIdentityGroup
            $ groupBy (\a b -> hammingDistance (fst a) (fst b) < thr) rs

  BSL.writeFile fp (A.encode rs')


------------------------------------------------------------------------------
-- | Compute default location for cache files, preparing parent directories
--   if necessary
defaultCacheFile :: IO FilePath
defaultCacheFile = do
  td <- getTemporaryDirectory
  createDirectoryIfMissing True $ concat [td, [pathSeparator], "fbp-cache"]
  return $
    concat [td, [pathSeparator], "fbp-cache", [pathSeparator], "hashes"]


------------------------------------------------------------------------------
-- | Toggle for using the existing cache, or generating a new one from
--   the given database and using the given identity grouping threshold
--   (explained in the haddock for @generateCache@)
data SearchCacheAction
  = UseCache
  | OverwriteCache PG.ConnectInfo IdentityGroupingThreshold
    -- ^ Building a new cache requires a database connection
    --   and an identity grouping threshold
  deriving (Show)


------------------------------------------------------------------------------
-- | Toggle for the type of output to produce
data SearchOutputFormat
 = OutputJSON
   -- ^ JSON blob
 | OutputHTML
   -- ^ HTML report
 deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Type wrapper over raw text to signal that text is a URL
newtype URL = URL { getUrl :: T.Text }
  deriving (Eq, A.FromJSON, A.ToJSON, Show)


------------------------------------------------------------------------------
-- | Type wrapper over an Int to track that it is meant for use as
--   phash grouping threshold (used during search tree generation)
newtype IdentityGroupingThreshold = IdentityGroupingThreshold Int
  deriving (Eq, Show)


------------------------------------------------------------------------------
data SearchOptions = SearchOptions
  { searchQuery       :: [Either FilePath URL]
    -- ^ FilePath or URL of images to search for
  , searchType        :: SearchType
  , chacheFile        :: Maybe FilePath
    -- ^ An overide for the location to store our search tree
  , overwriteCache    :: SearchCacheAction
  , outputFile        :: Maybe FilePath
  } deriving (Show)


------------------------------------------------------------------------------
data SearchType
  = SearchNearest
    -- ^ Return the nearest result
  | SearchKNearest Int
    -- ^ Return the nearest k results
  | SearchFirstInRanges [Int] Int
    -- ^ Return sample results from the list of distance boundaries,
    --   with at most n results per region
  deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Wrapper for responses to different search requests
--   Each one contains data from the query, to help with interpreting
--   the results in contexts where the query isn't available
--   TODO: Holding query here data is ugly. Fix this.
data SearchResults =
    NearestResult   (Either FilePath URL, PHash) (PHash, [URL])
  | KNearestResult  (Either FilePath URL, PHash) [(PHash, [URL])]
  | NearestInRanges (Either FilePath URL, PHash) [Int] [[(PHash, [URL])]]
  deriving (Show)


------------------------------------------------------------------------------
-- | Interpret a @SearchOptions@ as a set of instructions for fetching or
--   regenerating the kd-tree data, then running one of the possible
--   lookups from @SearchType@ (nearest, k-nearest, nearest-in-ranges)
runSearch :: SearchOptions -> IO [Either T.Text SearchResults]
runSearch (SearchOptions queries sType fp overwrite _) = do

  manager <- HTTP.newTlsManager

  -- compute default cache file location, or
  -- return the user's if they provided one
  cacheFilePath <- maybe defaultCacheFile return fp

  -- If requested, regenerate cache from ads database
  case overwrite of
    UseCache                   -> return ()
    OverwriteCache conn thresh -> generateCache conn thresh cacheFilePath

  SearchTree kdt <- loadCache cacheFilePath

  -- Iterate over all images requested
  for queries $ \(q :: Either FilePath URL) -> runExceptT $ do

    -- Compute requested image's phash
    imgFile :: FilePath <- either
      return
      (\(URL url) -> ExceptT $ downloadURLFile manager url)
      q

    -- imgFile <- either return (\(URL url) -> downloadURLFile manager url) q
    hash <- noteT "phash error" $ MaybeT $ imageHash imgFile

    return $ case sType of

    -- return $ case hashResult of
    --   Nothing   -> Left $ "pHash error for " <> T.pack imgFile
    --   Just hash -> case sType of

        -- @SearchNearest@ is handled by a simple call to the kd-tree library
        SearchNearest    -> NearestResult  (q, hash) (KD.nearest kdt hash)

        -- @SearchKNearest@ is handled by simple calls to kd-tree library
        SearchKNearest k -> KNearestResult (q, hash) (KD.kNearest kdt k hash)

        -- @SearchFirstInRange@ doesn't use kd-tree the library - instead it emits
        -- all contained points into a list sorted by similarity to the query, and
        -- consumes elememets from the list according to the requested distance
        -- ranges
        SearchFirstInRanges bounds k ->
          let ranges  = sort bounds
              hashes  = sortBy (comparing (hammingDistance hash . fst)) (KD.assocs kdt)
              rings   = scanl' (\(ps, lastRing) rad ->
                                  let (thisCandidates, nextCandidates) =
                                        break (\p -> hammingDistance (fst p) hash >= rad) ps
                                      thisRing = take k thisCandidates
                                  in  (nextCandidates, thisRing)
                               ) (hashes, mempty) ranges
              results = drop 1 $ fmap snd rings
          in  NearestInRanges (q, hash) ranges results
