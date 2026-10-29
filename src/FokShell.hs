{-# LANGUAGE OverloadedStrings,LambdaCase #-}
module FokShell where

import System.IO (hSetEcho, hSetBuffering, BufferMode (NoBuffering), hSetEncoding, utf8)

import Data.Functor

import System.Posix.Signals

import Lib.Keys
import FokShell.Types

import FokShell.Module (chainHook, initHook, postHook, preHook, chainEventHook, configureModule)
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Concurrent (forkIO, writeChan, newChan, Chan, readChan)
import FokShell.InputHandling (stdinEvent)
import Control.Monad.Reader (ReaderT(runReaderT), asks, MonadIO (liftIO))
import Control.Monad.Except (runExceptT)
import System.Posix (openFd, OpenMode (ReadWrite), defaultFileFlags, fdToHandle, getProcessGroupID, setProcessGroupIDOf, setTerminalProcessGroupID, getProcessID)

handleInterrupt :: Chan KeyEvent -> IO ()
handleInterrupt chan = do
  let ctrlCEvent = (control, Character "c")
  writeChan chan ctrlCEvent

fokshell :: FokShell -> IO ()
fokshell config = do
  envModules <- newTVarIO =<< mapM configureModule config.modules
  eventChan <- newChan
  ttyFd <- openFd "/dev/tty" ReadWrite defaultFileFlags
  ttyHandle <- fdToHandle ttyFd
  hSetEncoding ttyHandle utf8
  hSetBuffering ttyHandle NoBuffering
  hSetEcho ttyHandle False
  void $ installHandler sigTTOU Ignore Nothing
  void $ installHandler sigTTIN Ignore Nothing
  void $ installHandler sigINT (Catch $ handleInterrupt eventChan) Nothing
  ttyLock <- newTMVarIO ()
  void . forkIO . forever $ do
    _ <- atomically $ swapTMVar ttyLock ()
    event <- stdinEvent ttyHandle
    writeChan eventChan event
  pgid <- getProcessGroupID
  pid <- getProcessID
  setProcessGroupIDOf pid pgid
  setTerminalProcessGroupID ttyFd pgid
  let env = ShellEnv{envModules, eventChan, ttyFd, ttyHandle,pgid, ttyLock}
  (`runReaderT` env) $ do
    void . runExceptT $ (chainHook initHook :: ShellAction ())
    eventLoop

nextEvent :: Shell KeyEvent
nextEvent = liftIO . readChan =<< asks eventChan

eventLoop :: Shell ()
eventLoop = do
  event <- nextEvent
  dispatchEvent event
  eventLoop

dispatchEvent :: KeyEvent -> Shell () 
dispatchEvent key = do 
  _b <- runExceptT $ (chainEventHook preHook key :: ShellAction Bool)
  _b' <- runExceptT $ (chainEventHook postHook key :: ShellAction Bool)
  pure ()
