{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CliOptions where

import           Control.Monad
import           Control.Exception
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Database.PostgreSQL.Simple as PG
import           Options.Applicative
import           System.Environment
import           System.IO

getCommand :: IO Command
getCommand = execParser $ info (phashCommand <**> helper)
             ( fullDesc
             <> progDesc "Run commands for perceptual hashes against the ads database"
             )

data Command =
    DbTest           PG.ConnectInfo
  | DbResetHashes    PG.ConnectInfo
  | DbPopulateHashes PG.ConnectInfo
  deriving (Show)

phashCommand :: Parser Command
phashCommand = hsubparser $
     command "test-db"
     (info (DbTest <$> dbConn)
           (progDesc "Test connection to the ads database"))
  <> command "reset-phashes"
     (info (DbResetHashes <$> dbConn)
           (progDesc "Clear the phash column in the ads database"))
  <> command "populate-phashes"
     (info (DbPopulateHashes <$> dbConn)
           (progDesc "Compute phashes for images in the ads database"))

-- data DbConnCfg = DbConnCfg
--   { dbUser :: Text
--   , dbPass :: Text
--   , dbHost :: Text
--   , dbPort :: Int
--   , dbName :: Text
--   } deriving (Show)

dbConn :: Parser PG.ConnectInfo
dbConn =
  PG.ConnectInfo
  <$> strOption (long "dbhost"
                 <> short 'h'
                 <> help "Database Host"
                 <> value "localhost")
  <*> option auto (long "dbport"
                   <> short 'p'
                   <> help "Database Port"
                   <> value 5432)
  <*> strOption (long "dbuser"
                 <> short 'U'
                 <> help "Database User"
                 <> value "facebook_ads")
  <*> strOption (long "dbpass"
                 <> short 'p'
                 <> help "Database Password"
                 <> value "password")
  <*> strOption (long "dbname"
                 <> short 'd'
                 <> help "Database Name"
                 <> value "facebook_ads")

-- Extra Utilities for allowing CLI parser to sample env vars
-- and dotenv files
type Env = [(Text, Text)]

class FromText a where
  fromText :: Text -> Either String a

instance FromText Text where
  fromText = Right

environ :: (HasValue f, FromText a) => Text -> Env -> Mod f a
environ k env = maybe idm value . join $ parse <$> lookup k env
  where
    parse = either (const Nothing) Just . fromText

-- Read in all env vars and any vars from
-- an environment variable file
importEnv :: Maybe FilePath
             -- ^ Path to an environment variable,
             --   @Nothing@ will default to `~/.env`
          -> IO Env
importEnv envVarFile = do
    env     <- getEnvironment
    dotEnv  <- case envVarFile of
      Nothing ->
        (readFile ".env") `catch` (\(e :: SomeException) -> return "")
      Just fp ->
        readFile fp
    let fileEnv = getFileEnv dotEnv
        env'    = map (\(k,v) -> (T.pack k, T.pack v)) env
    return (env' ++ fileEnv)
  where

    getFileEnv :: String -> Env
    getFileEnv c = catMaybes . map splitPair . lines $ c

    stripLeadingSpace = dropWhile (\c -> elem c [' ', '\t'])

    -- Turn a line like
    -- "HOST=0.0.0.0:8080  #The host to listen on"
    -- into @Just ("HOST", "0.0.0.0:8080:")@
    -- or
    -- "#This is a comment"
    -- into @Nothing@
    splitPair :: String -> Maybe (Text, Text)
    splitPair l = case takeWhile (/= '#') l of
      "" -> Nothing
      l' -> let (key, val) = break (== '=') l'
            in  if   length key > 0
                     && length (stripLeadingSpace val) > 0
                then Just (T.pack key, T.pack (stripLeadingSpace val))
                else Nothing
