{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Binds where

import Lib.Keys
import FokShell.Module
import FokShell.Types
import FokShell.Utils
import Data.Functor
import Lib.Primitive
import Control.Monad.RWS (MonadReader(ask), MonadIO (liftIO))
import Control.Concurrent.STM (readTVarIO, atomically, readTVar)
import Control.Monad (forM_)
import System.Exit (exitSuccess)
import GHC.IO.Handle (hSetBuffering, hSetEcho, BufferMode (LineBuffering))
import System.Posix (fdToHandle)
import qualified Data.ByteString as BS
import FokShell.Module.Prompt (displayPrompt')
import qualified Data.Text.IO as T
import FokShell.Module.Input
import Lib.Format (moveCursor, Direction (DLeft), moveCursorRaw)
import qualified Data.Text as T

data Binds = Binds
  { binds :: [(KeyEvent, ShellAction ())]
  }

data BindsConfig = BindsConfig
  { binds :: [(KeyEvent, ShellAction ())]
  }

instance Def BindsConfig where
  def = BindsConfig
    [ ((control, Character "c"), resetShell)
    , ((control, Character "d"), exitShell)
    , ((control, Character "l"), clearShell)
    ]

resetShell :: ShellAction ()
resetShell = do
  env <- ask
  modules <- liftIO $ readTVarIO env.envModules
  forM_ modules resetHook

exitShell :: ShellAction ()
exitShell = do
  env <- ask
  modules <- liftIO $ readTVarIO env.envModules
  forM_ modules exitHook
  ttyHandle <- liftIO $ fdToHandle env.ttyFd
  liftIO $ hSetBuffering ttyHandle LineBuffering
  liftIO $ hSetEcho ttyHandle True
  liftIO exitSuccess

clearShell :: ShellAction ()
clearShell = do
  env <- ask
  liftIO $ BS.hPutStr env.ttyHandle "\ESC[H\ESC[J"
  displayPrompt'
  inputM <- get @Input
  (input, loc) <- liftIO . atomically $ (,) <$> readTVar inputM.input <*> readTVar inputM.cursorLocation
  liftIO . T.putStr $ input <> moveCursorRaw DLeft loc

instance Configurable Binds BindsConfig where
  configure BindsConfig{binds} = pure Binds {binds}

instance Module' Binds ShellAction where
  initHook _ = pure ()
  resetHook _ = pure True
  preHook m k = case lookup k m.binds of
    Just x -> x $> False
    Nothing -> pure True
  postHook _ _ = pure True
  exitHook _ = pure ()
