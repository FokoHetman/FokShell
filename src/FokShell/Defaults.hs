{-# LANGUAGE OverloadedStrings #-}
module FokShell.Defaults where
import Lib.Primitive
import FokShell.Module
import FokShell.Types
import Language.Parser
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Functor
import Lib.Keys
import FokShell.Module.JobManager
import FokShell.Module.TabCompletion
import FokShell.Module.Prompt
import System.Directory (getHomeDirectory)
import FokShell.Module.Preprocessor.StringPreprocessors (combineStringPreprocessors, substituter, envVarPreprocessor)
import Data.List (sort)
import FokShell.Module.History (withHomeDir, historyFile, HistoryModule)
import System.Exit (exitSuccess)

import FokShell.Builtin

import FokShell.Module qualified as Module
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

instance Def [Module ShellProcess] where
  def =
    [ Module (def :: PromptModule)
    , Module (def :: TabCompletion)
    , Module (def :: HistoryModule)
    , Module (def :: JobManagerModule)
    ]

instance Def [CompletionRule] where
  def = [
      --nix
      cdCompletion
    , fileListCompletion (const $ pure True) "cat"
    ]

instance Def [Builtin] where
  def = [
      cd
    , bmap
    , regex
    , set
    ]



haltAction :: Action
haltAction proc = putStrLn "^C" >> displayPrompt' proc $> proc {shellConfig = proc.shellConfig {input = "",cursorLoc=0}}

exitAction :: Action
exitAction p = Module.chainHook p.shellConfig.modules p Module.exitHook >> exitSuccess


clearAction :: Action
clearAction proc = putStrLn "\ESC[2J\ESC[H" *> displayPrompt' proc >> T.putStr proc.shellConfig.input >> hFlush stdout $> proc

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
    , completionRules = def
    , modules = def
    }


