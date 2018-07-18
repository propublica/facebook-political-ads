{-
Queries here drive the phash generation ETL job
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Queries (
    testDb
  , resetPhashes
  , populatePhashes
  , fetchSortedPhashes
  , downloadURLFile
  , countImageHashMisalignment 
  , testConnectInfo -- TODO temporary
  )where

--------------------------------------------------------------------------------
import           Control.Concurrent.STM               (TVar, atomically,
                                                       modifyTVar, newTVarIO,
                                                       readTVar, writeTVar)
import           Control.Exception                    (SomeException, try)
import           Data.LruCache                        as LRU
import           Data.Semigroup                       ((<>))
import           Control.Monad.Trans.Resource         (ResourceT, runResourceT)
import           Control.Monad                        (unless)
import qualified Data.ByteString.Lazy                 as BSL
import           Data.Int                             (Int64)
import           Data.Maybe                           (catMaybes)
import           Data.PHash                           (PHash(..), imageHash)
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.Types     as PG
import           Database.PostgreSQL.Simple.SqlQQ     (sql)
import           Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.Streaming as PGStream
import           Database.PostgreSQL.Simple.ToField   as PG
import qualified Network.HTTP.Client                  as HTTP
import qualified Network.HTTP.Client.TLS              as HTTP
import           Streaming                            (Stream, Of, chunksOf,
                                                       liftIO)
import qualified Streaming.Prelude                    as S
import qualified Streaming.Concurrent                 as S
import           System.Directory                     (getTemporaryDirectory,
                                                       createDirectoryIfMissing,
                                                       removePathForcibly)
import           System.Random                        (randomRIO)
import           System.FilePath                      (pathSeparator)
import           Text.Read                            (readMaybe)

------------------------------------------------------------------------------
-- | Check several assumptions about the database
--    - does it have the right table?
--    - with the right columns for images and phashes?
--    - with the right types?
testDb :: PG.ConnectInfo -> IO ()
testDb cfg = do
  conn   <- PG.connect cfg
  r      <- PG.query_ @(PG.Only Int) conn "select 1"
  unless (r == [PG.Only 1])
         (error $ "Strange result for query 'select 1': " ++ show r)
  schm   <- PG.query_ @(T.Text, T.Text) conn
            [sql| SELECT column_name, data_type
                  FROM information_schema.columns
                  WHERE table_name = 'ads'
                |]
  unless ( elem ("phash", "ARRAY") schm && elem ("images", "ARRAY") schm)
         (error $ "'ads table does not have 'phash' and 'images'\
                  \columns with correct type ARRAY\n\n" ++ show schm)


------------------------------------------------------------------------------
-- | Reset phashes column
resetPhashes :: PG.ConnectInfo -> IO ()
resetPhashes cfg = do
  conn <- PG.connect cfg
  r    <- PG.execute_ conn "UPDATE ads SET phash = null"
  print $ "Updated " ++ show (r :: Int64) ++ " records in ads database"


------------------------------------------------------------------------------
-- | Wrapper for id in the `ads` table
newtype AdId = AdId { getAdId :: T.Text }
  deriving (Eq, Ord, Show, PG.FromField, PG.ToField)


------------------------------------------------------------------------------
-- | Stream the image links from ads and write back the corresponding
--   perceptual hashes.
--   Several steps are performance sensitive, or may involve data
--   too large to reside in memory
--
--   Our strategy will be to stream image URLs into a work queue,
--   work-steal the URLs off the queue for (a) download and (b)
--   hashing, write the hashes into an lru-cache (keyed by URL),
--   and batch the results for `UPDATE` queries to the ads table.
--
--   TODO: Pull magic numbers into populate config CLI command
populatePhashes :: PG.ConnectInfo -> IO ()
populatePhashes cfg = do

  manager <- HTTP.newTlsManager
  conn <- PG.connect cfg

  hashCache <- newTVarIO (LRU.empty 1000000)

  -- Inbox/Outbox/ParallelWorker provided by `withBufferedTransform`
  PG.withTransaction conn $ runResourceT $ S.withBufferedTransform 5

    -- Parallel workers share per-ad phashing
    (adPhashes manager hashCache)

    -- Serial stream of incomig ads
    (S.writeStreamBasket $
     PGStream.stream_ conn "SELECT id, images FROM ads WHERE phash is null;")

    -- Serial chunked stream of db updates
    -- Chunking prevents us from doing a DB query
    -- per row written
    (\ob -> S.withStreamBasket ob
      (S.mapM_ (doInsert conn) . S.mapped S.toList . chunksOf 5))


------------------------------------------------------------------------------
-- | The per-ad worker serially computes phashes for all that ad's images
--   Take a LRU cache in a TVar (for sharing between threads), and
--   return a callback appropriate for use by @withBufferedTransform@.
--   The callback receives URLs from the OutBasket parameter,
--   downloads the images, computes their hashes, batches the results,
--   and writes them into the InBasket.
--
--   The LRU cache is updated by each record, and will be consulted before
--   downloading any URLs or computing their phashes. There is no TTL on
--   the cache, so if we want to invalidate it, we need to rerun the query
adPhashes :: HTTP.Manager
          -> TVar (LRU.LruCache T.Text (Either T.Text PHash))
          -> S.OutBasket (AdId, PG.PGArray T.Text)
          -> S.InBasket (AdId, PG.PGArray (Either T.Text PHash))
          -> ResourceT IO ()
adPhashes manager hashCache outBasket inBasket =
  S.withStreamBasket outBasket $ \outStream ->
    S.writeStreamBasket (S.mapM streamRow outStream) inBasket
  where
    streamRow (k, PG.PGArray urls) = do
      hashes <- mapM (liftIO . resolvePhash manager hashCache) urls
      return (k, PG.PGArray hashes)


------------------------------------------------------------------------------
-- | Insert a set of (Ad, phashes) pairs into the `ads` database
doInsert :: PG.Connection
         -> [(AdId, PG.PGArray (Either T.Text PHash))]
         -> ResourceT IO ()
doInsert dbConn phashes = do

  let formatEntry hashOrError = case  hashOrError of
        Left err        -> err
        Right (PHash h) -> T.pack (show h)
      inserts = fmap (\(k,v) -> (k, fmap formatEntry v)) $ phashes

  n <- liftIO $ PG.executeMany dbConn
    [sql| UPDATE ads
          SET phash = upd.phash
          FROM (VALUES (?,?)) as upd(id,phash)
          WHERE ads.id = upd.id
    |] inserts
  liftIO $ putStrLn $ "Writing " ++ show n ++ " records"


------------------------------------------------------------------------------
-- | For a URL, get the phash by either of two means:
--   1. Find it in the supplied cache
--   2. Download the URL, compute the phash, and update the cache
resolvePhash :: HTTP.Manager
             -> TVar (LRU.LruCache T.Text (Either T.Text PHash))
             -> T.Text
             -> IO (Either T.Text PHash)
resolvePhash manager hashCacheTVar url = do

  -- Check (and LRU-update) the cache in an STM transaction
  cacheSearch <- atomically $ do
    hashCache <- readTVar hashCacheTVar
    case LRU.lookup url hashCache of
      Nothing -> return Nothing
      Just ( cachedVal, newCache ) -> do
        writeTVar hashCacheTVar newCache
        return (Just cachedVal)

  case cacheSearch of
    Just hash -> return (hash)
    Nothing -> do

      dlPath <- downloadURLFile manager url
      case dlPath of
        Left  errMsg  -> return $ Left errMsg
        Right tmpPath -> do

          hash <- maybe (Left "pHash failure") Right <$> imageHash tmpPath
          removePathForcibly tmpPath

          atomically $ modifyTVar hashCacheTVar (LRU.insert url hash)

          return $  hash

downloadURLFile :: HTTP.Manager -> T.Text -> IO (Either T.Text FilePath)
downloadURLFile manager url = do

  dir <- getTemporaryDirectory
  createDirectoryIfMissing True $ concat [dir, [pathSeparator], "fbp-images"]

  -- Get a filepath
  -- TODO: Do this better. Temp filenames must be guaranteed unique and unixy
  tmpPath <-
    (\n -> concat [dir, [pathSeparator], "fbp-images", [pathSeparator], show n])
    <$> randomRIO @Int (1,100000)

  -- TODO: error handling
  httpReq <- HTTP.parseRequest (T.unpack url)
  dlResult <- try $ HTTP.httpLbs httpReq manager >>=
                    BSL.writeFile tmpPath . HTTP.responseBody

  return $ case dlResult of
    Left  (e :: SomeException) -> Left $ "Download Failure on: " <> url
    Right _                    -> Right tmpPath

-- phashURL :: HTTP.Manager -> T.Text -> IO (Either T.Text PHash)
-- phashURL manager url = do
--   imgFile <- downloadURLFile manager url
--   maybe (Left "pHash failure") Right <$> imageHash imgFile


-- For each ad, flatten the images and hashes
-- returning the list sorted by phash, with 0's removed
--
-- For example, if we have ads:
--
-- id: 111
-- images: { example.com/1.png, example.com/2.png }
-- phash:  { 123,               234               }
--
-- id: 222
-- images: { example.com/1.png, example.com/3.png }
-- phash:  { 123,             , 012               }
--
-- Then the query returns:
--
-- phash               url
-- 012                 example.com/3.png
-- 123                 example.com/1.png
-- 123                 example.com/1.png
-- 234                 example.com/2.png
fetchSortedPhashes :: PG.ConnectInfo -> IO [(PHash, T.Text)]
fetchSortedPhashes cfg = do

  conn <- PG.connect cfg
  rs <- PG.query_ conn
        [sql| SELECT ars.phash, ars.url
              FROM   (SELECT unnest(phash)  as phash,
                             unnest(images) as url
                      FROM ads) ars
              WHERE ars.phash != '0'
              AND   ars.phash IS NOT NULL
              ORDER BY ars.phash
            |]
  let readRow (hash, url) = case readMaybe hash of
        Nothing  -> Nothing
        Just w64 -> Just (PHash w64, url)
  return . catMaybes $ fmap readRow rs

countImageHashMisalignment :: PG.ConnectInfo -> IO [T.Text]
countImageHashMisalignment cfg = do
  conn <- PG.connect cfg
  r <- PG.query_ conn
        [sql| SELECT id
              FROM   (SELECT id,
                             coalesce(array_length(images,1), 0) as i,
                             coalesce(array_length(phash,1) , 0) as p
                      FROM ads
                      WHERE phash IS NOT NULL) lengths
              WHERE lengths.i != lengths.p
         |]
  return $ PG.fromOnly <$> r

testConnectInfo :: PG.ConnectInfo
testConnectInfo = PG.ConnectInfo "localhost" 5432 "fbpac" "password" "fbpac"
