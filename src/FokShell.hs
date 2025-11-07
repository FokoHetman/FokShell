{-# LANGUAGE OverloadedStrings,LambdaCase #-}
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
import Lib.Format

import Debug.Trace (traceShow, trace)
import Lib.Autocomplete (AutocompleteConfig(redrawHook, model))
import Data.List (singleton)

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
  
  extractedHistory <- getHistory config
  let proc = ShellProcess config {history = extractedHistory} InputOutput
  doStart <- startHook (hooks config) proc
  unless doStart exitSuccess

  shellProcRef <- newIORef proc

  done <- newEmptyMVar
  _ <- installHandler sigINT (Catch $ handleSignal shellProcRef done) Nothing

  displayPrompt $ prompt config $ colorScheme config
  updateCursorShape config
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
        DLeft   -> moveCursor' conf DLeft  1 $> ShellProcess (conf {cursorLoc = min (cursorLoc conf + 1) (T.length $ input conf)}) state
        DRight  -> moveCursor' conf DRight 1 $> ShellProcess (conf {cursorLoc = max (cursorLoc conf - 1) 0}) state
        Up      -> moveCursor' conf DLeft (T.length (input conf) - cursorLoc conf) >> 
          (\x ->  redrawFromCursor x {cursorLoc = T.length $ input x} >>  moveCursor' x {cursorLoc = T.length $ input x} DRight (T.length $ input x) $> ShellProcess x state) 
            (case historyIndex conf of
            Nothing -> case history conf of 
              []      -> conf
              (x:_)  -> conf {historyIndex = Just (0, input conf), input = x}
            Just (i, r) -> let j = min (length (history conf) - 1) (i+1) in conf {historyIndex = Just (j, r), input = history conf!!j}
            )
        Down      -> moveCursor' conf DLeft (T.length (input conf) - cursorLoc conf) >>
          (\x -> redrawFromCursor x {cursorLoc = T.length $ input x} >> moveCursor' x {cursorLoc = T.length $ input x} DRight (T.length $ input x) $> ShellProcess x state)
            (case historyIndex conf of
              Nothing -> conf
              Just (0, r) -> conf {historyIndex = Nothing, input = r}
              Just (i, r) -> let j = min (length (history conf) - 1) (i-1) in conf {historyIndex = Just (j, r), input = history conf!!j}
            )

    (KeyModifiers 0, Tab) ->  model (autocomplete conf) (input conf) (cursorLoc conf) (history conf) >>= (\case
          [] -> pure $ ShellProcess conf state
          [x]-> (putStr . T.unpack) (differ (input conf) x) >> hFlush stdout $> ShellProcess (replaceCurrent x conf) state
          (x:xs)  -> pure $ ShellProcess conf state
        ) . fst

    (KeyModifiers 1 {-control-}, Arrow d) -> case d of
        DLeft   -> moveCursor' conf DLeft (n DLeft) $> ShellProcess (conf {cursorLoc = cursorLoc conf + n DLeft}) state
        DRight  -> moveCursor' conf DRight (n DRight) $> ShellProcess (conf {cursorLoc = cursorLoc conf - n DRight}) state
        _ -> pure $ ShellProcess conf state
      where
        n DLeft = case T.words (snd $ T.splitAt (cursorLoc conf) $ T.reverse $ input conf) of
          (x:_)   -> T.length x 
          _       -> 0
        n DRight = case reverse $ T.words (fst $  T.splitAt (cursorLoc conf) $ T.reverse $ input conf) of
          (x:_)   -> T.length x 
          _       -> 0
        n _ = error "this should NEVER happen."
    
    (KeyModifiers 0, Enter) -> swallowPrompt (cursorLoc conf) (input conf) (prompt conf $ colorScheme conf) >> 
        putStrLn "" >> 
          handleJob (ShellProcess conf {history = T.strip (input conf):history conf, historyIndex = Nothing} state) 
        <* displayPrompt (prompt conf $ colorScheme conf)
    
    (KeyModifiers 0, Backspace) -> moveCursor' conf DLeft 1 >> redrawFromCursor nconf $> ShellProcess nconf state
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
  
  autocompleteOverrides out
  pure $ updateWithKey key out
  where
    replaceCurrent :: T.Text -> ShellConfig -> ShellConfig
    replaceCurrent with c = c {input = T.unwords new_ws}
      where
        ws = reverse $ T.words (input c)
        new_ws = take (curWordI c - 1) ws ++ with:take (length ws - curWordI c - 1) ws

    curWordI c = curWordI' (cursorLoc c) 0 $ T.words $ T.reverse (input c)
    curWordI' i y (x:xs) = if T.length x > i then y else curWordI' (i-T.length x) (y+1) xs
    curWordI' i y _ = y
   
    unwrapBind [x] defval = snd x defval
    unwrapBind [] defval = pure defval
    unwrapBind _ _ = undefined
    addToInput c t = c {input = T.concat [left, t, right]}
      where
        loc = cursorLoc c 
        inp = input c
        right = T.reverse $ T.take loc $ T.reverse inp
        left  = T.take (T.length inp - T.length right) inp

    autocompleteOverrides (ShellProcess c _) = model (autocomplete c) (input c) (cursorLoc c) (history c) >>= redrawHook (autocomplete c) (input c) (cursorLoc c) (colorScheme c)



