{-# LANGUAGE OverloadedStrings,LambdaCase #-}
module FokShell where

import qualified Data.Text as T
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode (NoBuffering), hFlush, stdout)

import Data.Functor

import System.Posix.Signals
import Control.Concurrent.MVar
import Data.IORef (newIORef, IORef, writeIORef, readIORef)

import FokShell.Utils
import Lib.Format
--import Lib.Autocomplete
import Lib.Primitive
import Lib.Keys
import FokShell.Types

import FokShell.Module (chainHook, initHook, postHook, preHook, chainEventHook, Configurable (configure), configureModule)
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Concurrent (forkIO, writeChan, newChan, Chan, readChan)
import FokShell.InputHandling (stdinEvent)
import Control.Monad.Reader (ReaderT(runReaderT), asks, MonadIO (liftIO))
import Control.Monad.Except (runExceptT)


handleInterrupt :: Chan KeyEvent -> IO ()
handleInterrupt chan = do
  let ctrlCEvent = (control, Character "c")
  writeChan chan ctrlCEvent

fokshell :: FokShell -> IO ()
fokshell config = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering

  envModules <- newTVarIO =<< mapM configureModule config.modules
  eventChan <- newChan
  void . forkIO . forever $ do
    event <- stdinEvent
    writeChan eventChan event

  -- todo: make it a module horray
  _ <- installHandler sigINT (Catch $ handleInterrupt eventChan) Nothing
  let env = ShellEnv{envModules, eventChan}
  (`runReaderT` env) $ do
    void . runExceptT $ (chainHook initHook :: ShellAction ())
    eventLoop
nextEvent :: Shell KeyEvent
nextEvent = liftIO . readChan =<< asks eventChan

eventLoop :: Shell ()
eventLoop = do
  event <- nextEvent
  parseEvent event
  eventLoop

parseEvent :: KeyEvent -> Shell () 
parseEvent key = do 
  _b <- runExceptT $ (chainEventHook preHook key :: ShellAction Bool)
  {-config <- atomically $ readTVar conf
  when b $ do
    case key of
      (KeyModifiers 0, Arrow d) -> case d of
          DLeft   -> moveCursor' conf DLeft  1
          DRight  -> moveCursor' conf DRight 1
          _ -> pure ()
      (KeyModifiers 0, Backspace) -> moveCursor' conf DLeft 1 >> redrawFromCursor nconfig >> atomically (writeTVar conf nconfig)
        where
          right = T.reverse $ T.take config.cursorLoc $ T.reverse config.input
          left  = T.take (T.length config.input - T.length right) config.input
          nconfig = config { input = T.concat [T.dropEnd 1 left, right]}
      (_, Delete) -> redrawFromCursor nconfig >> atomically (writeTVar conf nconfig)
        where
          right = T.reverse $ T.take config.cursorLoc $ T.reverse config.input
          left  = T.take (T.length config.input - T.length right) config.input
          nconfig = config { input = T.concat [left, T.drop 1 right], cursorLoc = max 0 $ config.cursorLoc - 1}
      (_, Character "\NUL") -> pure () -- why do I handle this lmao
      (KeyModifiers 0, Character rawKey) -> do
        putStr $ T.unpack rawKey
        hFlush stdout
        redrawFromCursor config
        atomically $ modifyTVar conf $ (`addToInput` rawKey)
      _ -> do 
        let bind = filter (\x -> fst x == key) $ binds config
        case bind of
          (x:_) -> snd x conf
          _ -> pure ()-}
  _b' <- runExceptT $ (chainEventHook postHook key :: ShellAction Bool)
  --config <- atomically $ readTVar conf
  pure ()
  {-atomically $ writeTVar conf config {lastEvent = key}
    where
    addToInput c t = c {input = T.concat [left, t, right]}
      where
        loc = cursorLoc c 
        inp = input c
        right = T.reverse $ T.take loc $ T.reverse inp
        left  = T.take (T.length inp - T.length right) inp-}
    
    {-executablelist' :: ShellProcess -> [T.Text]
    executablelist' p = maybe [] (fromMaybe [] . fromDynamic) (lookupCache (shellCache p) "executables" >>= \x -> lookupCache x "execs")-}

