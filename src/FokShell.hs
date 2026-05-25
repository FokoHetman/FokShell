{-# LANGUAGE OverloadedStrings,LambdaCase #-}
module FokShell where

import FokShell.InputHandling (nextEvent)


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

import Data.Dynamic (fromDynamic)
import Data.Maybe (fromMaybe)
import FokShell.Module (chainHook, initHook, postHook, preHook, chainEventHook)
import Control.Concurrent.STM
import Control.Monad (when)


handleInterrupt :: TVar ShellConfig -> MVar () -> IO ()
handleInterrupt configVar done = do
  let ctrlCEvent = (control, Character "c")
  parseEvent configVar ctrlCEvent
  putMVar done ()

fokshell :: ShellConfig -> IO ()
fokshell config = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  configVar <- newTVarIO config
  --proc <- updatePath $ ShellProcess {shellConfig = config, shellState = InputOutput, shellCache = Cache []}
  chainHook configVar initHook
  --shellProcRef <- newIORef p --{shellConfig = p.shellConfig {modules = modules}}

  done <- newEmptyMVar
  _ <- installHandler sigINT (Catch $ handleInterrupt configVar done) Nothing
  eventLoop configVar

eventLoop :: TVar ShellConfig -> IO ()
eventLoop configVar = do

  -- implement reversion (event -> string), or also pass the raw string here.
  -- this will help with a job handler.
  event <- nextEvent
  parseEvent configVar event
  eventLoop configVar

parseEvent :: TVar ShellConfig -> KeyEvent -> IO () 
parseEvent conf key = do 
  b <- chainEventHook conf preHook key
  config <- atomically $ readTVar conf
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
          _ -> pure ()
  b' <- chainEventHook conf postHook key
  config <- atomically $ readTVar conf
  atomically $ writeTVar conf config {lastEvent = key}
    where
    addToInput c t = c {input = T.concat [left, t, right]}
      where
        loc = cursorLoc c 
        inp = input c
        right = T.reverse $ T.take loc $ T.reverse inp
        left  = T.take (T.length inp - T.length right) inp
    
    executablelist' :: ShellProcess -> [T.Text]
    executablelist' p = maybe [] (fromMaybe [] . fromDynamic) (lookupCache (shellCache p) "executables" >>= \x -> lookupCache x "execs")

