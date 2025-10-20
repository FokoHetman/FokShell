{-# LANGUAGE OverloadedStrings #-}
module FokShell where

import InputHandling (nextEvent)

import qualified Data.Text as T
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (when, unless)
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode (NoBuffering), hFlush, stdout)
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Posix (getEffectiveUserName)

import Data.Functor

import System.Posix.Signals
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Data.IORef (newIORef, IORef, writeIORef, readIORef)

import Network.HostName
import JobManager

import ExposedTypes

-- TODO:
-- handle printing prompt with input, cursor, etc
-- job mgr



handleSignal :: IORef ShellProcess -> MVar () -> IO ()
handleSignal shellProcRef done = do
  proc <- readIORef shellProcRef
  let ctrlCEvent = (control, Character "c")
  newProc <- parseEvent proc ctrlCEvent
  writeIORef shellProcRef newProc
  putMVar done ()

fokshell :: ShellConfig -> IO ()
fokshell config = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering

  let proc = ShellProcess config InputOutput
  doStart <- startHook (hooks config) proc
  unless doStart exitSuccess

  shellProcRef <- newIORef proc

  done <- newEmptyMVar
  _ <- installHandler sigINT (Catch $ handleSignal shellProcRef done) Nothing

  displayPrompt $ prompt config
  eventLoop shellProcRef


eventLoop :: IORef ShellProcess -> IO ()
eventLoop procRef = do

  -- implement reversion (event -> string), or also pass the raw string here.
  -- this will help with a job handler.
  event <- nextEvent
  proc <- readIORef procRef
  newProc <- parseEvent proc event
  writeIORef procRef newProc
  eventLoop procRef


parseEvent :: ShellProcess -> KeyEvent -> IO ShellProcess 
parseEvent (ShellProcess conf state) key = do 
  out <- case key of
  -- KEYS
    (KeyModifiers 0, Arrow d) -> case d of
        DLeft   -> moveCursor' conf CLeft  1 $> ShellProcess (conf {cursorLoc = min (cursorLoc conf + 1) (T.length $ input conf)}) state
        DRight  -> moveCursor' conf CRight 1 $> ShellProcess (conf {cursorLoc = max (cursorLoc conf - 1) 0}) state
        _       -> undefined
    (KeyModifiers 0, Enter) -> putStrLn "" >> handleJob (ShellProcess conf state) <* displayPrompt (prompt conf)
    (KeyModifiers 0, Backspace) -> moveCursor' conf CLeft 1 >> redrawFromCursor nconf $> ShellProcess nconf state
      where
        loc = cursorLoc conf 
        inp = input conf
        right = T.reverse $ T.take loc $ T.reverse inp
        left  = T.take (T.length inp - T.length right) inp
        nconf = conf { input = T.concat [T.dropEnd 1 left, right]}
    (_, Delete) -> redrawFromCursor nconf $> ShellProcess nconf state
      where
        loc = cursorLoc conf 
        inp = input conf
        right = T.reverse $ T.take loc $ T.reverse inp
        left  = T.take (T.length inp - T.length right) inp
        nconf = conf { input = T.concat [left, T.drop 1 right], cursorLoc = max 0 $ loc - 1}
    (KeyModifiers 0, Character rawKey) -> do
      putStr $ T.unpack rawKey
      hFlush stdout
      redrawFromCursor conf
      pure $ ShellProcess (addToInput conf rawKey) state
    _ -> do 
      let bind = filter (\x -> fst x == key) $ binds conf
      unwrapBind bind $ ShellProcess conf state
  pure $ updateWithKey key out
  where
    unwrapBind [x] defval = snd x defval
    unwrapBind [] defval = pure defval
    unwrapBind _ _ = undefined
    addToInput c t = c {input = T.concat [left, t, right]}
      where
        loc = cursorLoc c 
        inp = input c
        right = T.reverse $ T.take loc $ T.reverse inp
        left  = T.take (T.length inp - T.length right) inp
