{- This is the Intended Main entrypoint for the cli,
   The 'real' main (which calls this one) is in `exec/Main.hs`.
   The library has a main-like function so that it can be shared
   with a test suite
-}

module RunCli (
    runCommand
  , main
  ) where

import CliOptions
import Queries

-------------------------------------------------------------------------------
-- | Passthrough from CLI inputs to queries
runCommand :: Command -> IO ()
runCommand (DbTest cfg)           = testDb          cfg
runCommand (DbResetHashes cfg)    = resetPhashes    cfg
runCommand (DbPopulateHashes cfg) = populatePhashes cfg

-------------------------------------------------------------------------------
-- | Convenience entrypoint for main
main :: IO ()
main = getCommand >>= runCommand
