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
  )where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Data.LruCache                        as LRU
import           Control.Monad.Trans.Resource
import           Control.Monad.Except                 (runExceptT)
import           Control.Concurrent.STM.TBQueue
import           Control.Monad                        (unless)
import qualified Data.ByteString.Char8                as BS
import qualified Data.ByteString.Lazy                 as BSL
import           Data.Int                             (Int64)
import qualified Data.Traversable                     as T
import           Data.PHash
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as E
import           Data.Typeable
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.Types     as PG
import           Database.PostgreSQL.Simple.SqlQQ     (sql)
import           Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.Streaming as PGStream
import           Database.PostgreSQL.Simple.ToField   as PG
import           Database.PostgreSQL.Simple           (Only(..), connect)
import qualified Network.HTTP.Client                  as HTTP
import qualified Network.HTTP.Client.TLS              as HTTP
import           Streaming
import qualified Streaming                            as S
import qualified Streaming.Prelude                    as S
import           Streaming.Concurrent
import           System.Directory
import           System.Random
import           System.FilePath

import           CliOptions

------------------------------------------------------------------------------
-- | Check several assumptions about the database
--    - does it have the right table?
--    - with the right columns for images and phashes?
--    - with the right types?
testDb :: PG.ConnectInfo -> IO ()
testDb cfg = do
  conn   <- connect cfg
  r      <- PG.query_ @(Only Int) conn "select 1"
  unless (r == [Only 1])
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
  conn <- connect cfg
  r    <- PG.execute_ conn "UPDATE ads SET phash = '{}'"
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
  conn <- connect cfg

  hashCache <- newTVarIO (LRU.empty 1000000)

  -- Inbox/Outbox/ParallelWorker provided by `withBufferedTransform`
  runResourceT $ withBufferedTransform 5

    -- Parallel workers share per-ad phashing
    (adPhashes manager hashCache)

    -- Serial stream of incomig ads
    (writeStreamBasket $
     PGStream.stream_ conn "SELECT id, images FROM ads WHERE phash = '{}';")

    -- Serial chunked stream of db updates
    -- Chunking prevents us from doing a DB query
    -- per row written
    (\ob -> withStreamBasket ob
      (S.mapM_ (doInsert conn) . S.mapped S.toList . S.chunksOf 5))


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
          -> OutBasket (AdId, PG.PGArray T.Text)
          -> InBasket (AdId, PG.PGArray (Either T.Text PHash))
          -> ResourceT IO ()
adPhashes manager hashCache outBasket inBasket =
  withStreamBasket outBasket $ \outStream ->
    let r = S.mapM (\(k,PG.PGArray urls) -> do
                       hashes <- mapM (liftIO . resolvePhash manager hashCache) urls
                       return (k, PG.PGArray hashes)
                   ) outStream
    in writeStreamBasket r inBasket



------------------------------------------------------------------------------
-- | Insert a set of (Ad, phashes) pairs into the `ads` database
doInsert :: PG.Connection -> [(AdId, PG.PGArray (Either T.Text PHash))] -> ResourceT IO ()
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

      -- Get a filepath
      -- TODO: Do this better. Temp filenames must be guaranteed unique and unixy
      tmpPath <-
        (\dir n -> concat [dir, [pathSeparator], "fbp-image-", show n])
        <$> getTemporaryDirectory
        <*> randomRIO @Int (1,100000)

      -- TODO: error handling
      httpReq <- HTTP.parseRequest (T.unpack url)
      liftIO $ BSL.writeFile tmpPath . HTTP.responseBody =<< HTTP.httpLbs httpReq manager

      hash <- maybe (Left "pHash failure") Right <$> imageHash tmpPath
      removePathForcibly tmpPath

      atomically $ modifyTVar hashCacheTVar (LRU.insert url hash)

      return $  hash


------------------------------------------------------------------------------
-- | **Old**  Yesterday's implemetation here was single-threaded in the worker
--   Take a LRU cache in a TVar (for sharing between threads), and
--   return a callback appropriate for use by @withBuffer@. The callback
--   receives URLs from the @Stream (Of [(AdId, PGArray T.Text)]) m a@ parameter,
--   downloads the images, computes their hashes, batches the results,
--   and writes them into the database.
--
--   The LRU cache is updated by each record, and will be consulted before
--   downloading any URLs or computing their phashes. There is no TTL on
--   the cache, so if we want to invalidate it, we need to rerun the query
handleRecords :: PG.Connection
              -> HTTP.Manager
              -> TVar (LRU.LruCache T.Text (Either T.Text PHash))
              -> Stream (Of (AdId, PG.PGArray T.Text)) (ResourceT IO) ()
              -> ResourceT IO ()
handleRecords dbConn manager hashCache urlStream = do

  liftIO (putStrLn "handleRecords")

  -- For each ads row in the stream, sequence all row's images through phash
  let r = S.mapM  (\(k, PG.PGArray urls) -> do
                      hashes <- mapM (liftIO . resolvePhash manager hashCache) urls
                      return (k, PG.PGArray hashes)
                  ) urlStream

  -- Collect groups of n rows into chunks for bulk DB insert
  -- For now, n == 5
  S.mapM_ (doInsert dbConn) $ S.mapped S.toList $ S.chunksOf 5 r
