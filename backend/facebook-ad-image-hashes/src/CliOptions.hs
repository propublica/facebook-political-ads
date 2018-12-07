{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CliOptions where

import           Control.Monad
import           Control.Exception
import           Data.Maybe
import           Data.List.NonEmpty  (NonEmpty(..))
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Database.PostgreSQL.Simple as PG
import           Options.Applicative
import           System.Environment
import           System.IO

import           Search

getCommand :: IO Command
getCommand = execParser $ info (pCommand <**> helper)
             ( fullDesc
             <> progDesc "Run commands for perceptual hashes against the ads database"
             )

data Command
  = CmdDbTest         PG.ConnectInfo
  | CmdResetHashes    PG.ConnectInfo
  | CmdPopulateHashes PG.ConnectInfo
  | CmdSearch         SearchOptions
  deriving (Show)


pCommand :: Parser Command
pCommand =
      (hsubparser $
          command "test-db"
          (info (CmdDbTest <$> dbConn)
                (progDesc "Test connection to the ads database"))
       <> command "reset-phashes"
          (info (CmdResetHashes <$> dbConn)
                (progDesc "Clear the phash column in the ads database"))
       <> command "populate-phashes"
          (info (CmdPopulateHashes <$> dbConn)
                (progDesc "Compute phashes for images in the ads database"))
       <> command "search"
          (info (CmdSearch <$> searchParser)
                (progDesc "Search for similar images"))
      )


searchParser :: Parser SearchOptions
searchParser = SearchOptions
  <$> some (
            (fmap Left (strOption (long "filepath" <> help "Filepath to query"))
             <|>
             fmap (Right . URL) (strOption (long "url" <> help "URL to query"))
            )
           )
  <*> searchTypeParser
  <*> (fmap Just (option auto (long "cache-file"
                               <> help "Cache filepath")
                 )
       <|> pure Nothing)
  <*> (( flag' True (long "overwrite-cache") *>
        ((\db thr -> OverwriteCache db thr) <$>
         dbConn <*>
         fmap IdentityGroupingThreshold (option auto (long "threshold"))
        )
      ) <|> pure UseCache)
  <*> (fmap Just (strOption (long "out" <> help "Generate report in html (with .htm or .html suffix) or json")) <|> pure Nothing)


searchTypeParser :: Parser SearchType
searchTypeParser =
     fmap SearchKNearest
       (option auto (long "k-nearest" <> help "Get the k nearest results"))
  <|> (SearchFirstInRanges
       <$> option auto (long "range-bounds" <>
                        help "List of boundaries for concentric ring search")
       <*> option auto (long "n-examples" <>
                        help "Number of examples per ring range")
      )
  <|> pure SearchNearest


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
