{-# LANGUAGE OverloadedStrings #-}
module FokShell.Defaults where
import Lib.Primitive
import FokShell.Module
import FokShell.Types
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lib.Keys
import FokShell.Module.JobManager
import FokShell.Module.TabCompletion
import FokShell.Module.Prompt
import FokShell.Module.History (History)
import System.Exit (exitSuccess)

import FokShell.Builtin

import FokShell.Module qualified as Module
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import FokShell.Module.Parser (ParserModule)
import FokShell.Module.DirectoryTracker (DirectoryTracker)
import Control.Concurrent.STM (STM, modifyTVar, atomically, readTVarIO)

instance Def [Module ShellConfig] where
  def =
    [ {-module' (def :: DirectoryTracker)
    , module' (def :: Prompt)
    , module' (def :: TabCompletion)
    , module' (def :: History)
    , module' (def :: JobManager)
    , module' (def :: ParserModule)-}
    ]

instance Def [CompletionRule] where
  def = [
      --nix
      cdCompletion
    , fileListCompletion (const $ pure True) "cat"
    ]

instance Def (Map.Map T.Text Builtin) where
  def = Map.fromList $ [
      ("cd", cd)
    , ("regex", regex)
    ]



haltAction :: Action
haltAction proc = do
  chainHook proc resetHook
  putStrLn "^C"
  displayPrompt' =<< readTVarIO proc
  atomically . modifyTVar proc $ \p -> p {input = "",cursorLoc=0}

exitAction :: Action
exitAction p = Module.chainHook p Module.exitHook >> exitSuccess


clearAction :: Action
clearAction proc = do
  putStrLn "\ESC[2J\ESC[H"
  proc' <- readTVarIO proc
  displayPrompt' proc'
  T.putStr proc'.input
  hFlush stdout

instance Def [(KeyEvent, Action)] where
  def = [
        ((control, Character "c"), haltAction)
      , ((control, Character "d"), exitAction)
      , ((control, Character "l"), clearAction)
    ]

instance Def ShellConfig where
  def = ShellConfig
    { input = ""
    , cursorLoc = 0
    , binds = def
    , lastEvent = (KeyModifiers 0, Escape)
    , trigger = (KeyModifiers 0, Escape)
    , builtins = def
    , modules = def
    }


