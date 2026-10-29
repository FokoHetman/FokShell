{-# LANGUAGE OverloadedStrings #-}
module FokShell.Defaults where
import Lib.Primitive
import FokShell.Module
import FokShell.Types
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lib.Keys
import FokShell.Utils
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
import Control.Concurrent.STM (STM, modifyTVar, atomically, readTVarIO, writeTVar)
import FokShell.Module.Input
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))

instance Def [Module ShellAction] where
  def =
    [ {-module' (def :: DirectoryTracker)
    , module' (def :: Prompt)
    , module' (def :: TabCompletion)
    , module' (def :: History)
    , module' (def :: JobManager)
    , module' (def :: ParserModule)-}
    ]
{-
instance Def [CompletionRule] where
  def = [
      --nix
      cdCompletion
    , fileListCompletion (const $ pure True) "cat"
    ]
-}

haltAction :: ShellAction ()
haltAction = do
  void $ chainBoolHook resetHook
  --putStrLn "^C"
  --displayPrompt' =<< readTVarIO proc
  with @Input $ \inp -> liftIO . atomically $ do
    writeTVar inp.input "" 
    writeTVar inp.cursorLocation 0

exitAction :: ShellAction ()
exitAction = Module.chainHook Module.exitHook >> liftIO exitSuccess


clearAction :: ShellAction ()
clearAction = do
  liftIO $ putStrLn "\ESC[2J\ESC[H"
  displayPrompt'
  input <- liftIO . readTVarIO . (.input) =<< get @Input
  liftIO $ T.putStr input
  liftIO $ hFlush stdout
{-
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
-}
