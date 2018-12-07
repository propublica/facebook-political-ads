{- This is the Intended Main entrypoint for the cli,
   The 'real' main (which calls this one) is in `exec/Main.hs`.
   The library has a main-like function so that it can be shared
   with a test suite
-}

module RunCli (
    runCommand
  , main
  ) where

import Data.List (isSuffixOf)
import System.IO (writeFile)

import CliOptions
import Queries
import Search
import Report

-------------------------------------------------------------------------------
-- | Passthrough from CLI inputs to queries
runCommand :: Command -> IO ()
-- runCommand = print
runCommand (CmdDbTest cfg)         = testDb          cfg
runCommand (CmdResetHashes cfg)    = resetPhashes    cfg
runCommand (CmdPopulateHashes cfg) = populatePhashes cfg
runCommand (CmdSearch opts)        =
  runSearch opts >>= report (outputFile opts)

  where report Nothing = print
        report (Just fp) | ".html" `isSuffixOf` fp = writeFile fp . htmlReport
                         | ".htm"  `isSuffixOf` fp = writeFile fp . htmlReport
                         | otherwise                 = print

-------------------------------------------------------------------------------
-- | Convenience entrypoint for main
main :: IO ()
main = getCommand >>= runCommand
