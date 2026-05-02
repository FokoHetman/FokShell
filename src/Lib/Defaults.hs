{-# LANGUAGE OverloadedStrings #-}
module Lib.Defaults where
import Lib.Primitive
import FokShell.Module
import Lib.Config
import FokShell.Module.TabCompletion
import Language.Parser
import System.Posix (isRegularFile, getFileStatus)

import Data.Text qualified as T
import Data.Functor
import Lib.Keys
import Lib.Format (getFormattedDirectory)
import FokShell.Module.JobManager
import System.Directory (getHomeDirectory)
import FokShell.Module.Preprocessor.StringPreprocessors (combineStringPreprocessors, substituter, envVarPreprocessor)
import Data.List (sort)
import FokShell.Module.History (withHomeDir, historyFile)

instance Def [Module ShellProcess] where
  def =
    [ Module TabCompletion {mode = Disabled, selected = Nothing, completions = [], autocomplete = def, maxSuggestions = 10, shadowText = True, sortAlgorithm = const sort}
    , Module $ historyFile (withHomeDir ".config/fokshell/history") 10000
    , Module JobManagerModule {jobs = [], preprocessors = [combineStringPreprocessors [substituter "~" (T.pack <$> getHomeDirectory) 1, envVarPreprocessor]]}
    ]

instance Def [CompletionRule] where
  def = [
      --nix
      cdCompletion
    , fileListCompletion (const $ pure True) "cat"
    ]

instance Def CursorConfig where
  def = CursorConfig { cursorShape = BlinkingBar }

instance Def [Builtin] where
  def = [
      cd
    --, wrapped
    , bmap
    , regex
    , table
    ]

instance Def ShellHooks where
  def = ShellHooks
    { haltHook  = defaultHaltHook
    , exitHook  = defaultExitHook
    , startHook = defaultStartHook
    , clearHook = defaultClearHook
    }

instance Def [(KeyEvent, Action)] where
  def = [
        ((control, Character "c"), \proc -> proc.shellConfig.hooks.haltHook proc >>= \x -> if x then haltAction proc else pure proc)
      , ((control, Character "d"), \proc -> proc.shellConfig.hooks.exitHook proc >>= \x -> if x then exitAction proc else pure proc)
      , ((control, Character "l"), \proc -> proc.shellConfig.hooks.clearHook (proc {shellConfig = (shellConfig proc) {trigger=(control, Character "l")}}) >>= \x -> if x then clearAction proc else pure proc)
    ]

instance Def ShellConfig where
  def = ShellConfig
    { hooks = def
    , prompt = const $ SingleLine (getFormattedDirectory <&> (<> " > ")) Never
    , input = ""

    , cursorLoc = 0

    , colorScheme = def
    , binds = def
    , lastEvent = (KeyModifiers 0, Escape)
    , trigger = (KeyModifiers 0, Escape)
    , jobManager = JobMgr []
    , builtins = def

    --, autocomplete = def
    , cursorConfig = def
    , completionRules = def

    , modules = def
    }


