{-# LANGUAGE OverloadedStrings,LambdaCase #-}
module FokShell where

import FokShell.InputHandling (nextEvent)
import FokShell.JobManager

import qualified Data.Text as T
import System.Exit (exitSuccess)
import Control.Monad (when, unless)
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode (NoBuffering), hFlush, stdout)

import Data.Functor

import System.Posix.Signals
import Control.Concurrent.MVar
import Data.IORef (newIORef, IORef, writeIORef, readIORef)

import FokShell.JobManager
import FokShell.Utils
import Lib.Format
--import Lib.Autocomplete
import Lib.Primitive
import Lib.Keys
import FokShell.Types

import Data.Dynamic (fromDynamic)
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import Language.Parser (Parser(runParser))
--import Language.Autocomplete
import Debug.Trace (traceShow)
import FokShell.Module (chainHook, initHook, postHook, preHook, chainEventHook)


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
  
  proc <- updatePath $ ShellProcess {shellConfig = config, shellState = InputOutput, shellCache = Cache []}
  p <- chainHook proc initHook
  shellProcRef <- newIORef p --{shellConfig = p.shellConfig {modules = modules}}

  done <- newEmptyMVar
  _ <- installHandler sigINT (Catch $ handleSignal shellProcRef done) Nothing
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


bool' a b c = bool b c a

parseEvent :: ShellProcess -> KeyEvent -> IO ShellProcess 
parseEvent proc' key = do 
  (b, proc) <- chainEventHook proc' preHook key
  out <- bool' b (pure proc) $ do
    let conf = shellConfig proc
    let state = shellState proc
    let cache = shellCache proc
    case key of
      -- KEYS
      (KeyModifiers 0, Arrow d) -> case d of
          DLeft   -> moveCursor' conf DLeft  1 $> proc {shellConfig = conf {cursorLoc = min (cursorLoc conf + 1) (T.length $ input conf)}}
          DRight  -> moveCursor' conf DRight 1 $> proc {shellConfig = conf {cursorLoc = max (cursorLoc conf - 1) 0}}
          _ -> pure proc
          {-Up      -> when (T.length (input conf) - cursorLoc conf > 0 ) (moveCursor' conf DLeft (T.length (input conf) - cursorLoc conf)) >> 
            (\x ->  redrawFromCursor x {cursorLoc = T.length $ input x} >>  moveCursor' x {cursorLoc = T.length $ input x} DRight (T.length $ input x) $> proc {shellConfig = x {cursorLoc = 0}})
              (case historyIndex conf of
              Nothing -> case history conf of 
                []      -> conf
                (x:_)  -> conf {historyIndex = Just (0, input conf), input = x}
              Just (i, r) -> let j = min (length (history conf) - 1) (i+1) in conf {historyIndex = Just (j, r), input = history conf!!j}
              )
          Down      -> when (T.length (input conf) - cursorLoc conf > 0 ) (moveCursor' conf DLeft (T.length (input conf) - cursorLoc conf)) >>
            (\x -> redrawFromCursor x {cursorLoc = T.length $ input x} >> moveCursor' x {cursorLoc = T.length $ input x} DRight (T.length $ input x) $> proc {shellConfig = x {cursorLoc = 0}})
              (case historyIndex conf of
                Nothing -> conf
                Just (0, r) -> conf {historyIndex = Nothing, input = r}
                Just (i, r) -> let j = max 0 (i-1) in conf {historyIndex = Just (j, r), input = history conf!!j}
              )-}

      {-(KeyModifiers 0, Tab) -> bool (pure proc) (model (conf.autocomplete) (moddata conf) >>= (\case
            [] -> pure proc
            [x] -> {- kindly assume curWord len >0 -} moveCursor DLeft (T.length $ curWord conf) >> putStrf x >> hFlush stdout $> proc {shellConfig = replaceCurrent x conf}
            -- todo: pre-hook, modularize this
            (x:xs) -> displaySuggestions (x:xs) $> proc {shellConfig = conf {tabMode = Selection, tabSuggestions = x:xs, tabIndex = 0}}
          ) . fst) (T.null (curWord conf) && T.length (curWord conf) - curWordI conf == 0)-}

      {-(KeyModifiers 4 {-control-}, Arrow d) -> case d of
          DLeft   -> moveCursor' conf DLeft (n DLeft) $> proc {shellConfig = conf {cursorLoc = cursorLoc conf + n DLeft}}
          DRight  -> moveCursor' conf DRight (n DRight) $> proc {shellConfig = conf {cursorLoc = cursorLoc conf - n DRight}}
          _ -> pure proc
        where
          n DLeft = case T.words (snd $ T.splitAt (cursorLoc conf) $ T.reverse $ input conf) of
            (x:_)   -> T.length x 
            _       -> 0
          n DRight = case reverse $ T.words (fst $  T.splitAt (cursorLoc conf) $ T.reverse $ input conf) of
            (x:_)   -> T.length x 
            _       -> 0
          n _ = error "this should NEVER happen."-}
    
      (KeyModifiers 0, Backspace) -> moveCursor' conf DLeft 1 >> redrawFromCursor nconf $> proc {shellConfig = nconf}
        where
          loc = cursorLoc conf 
          inp = input conf
          right = T.reverse $ T.take loc $ T.reverse inp
          left  = T.take (T.length inp - T.length right) inp
          nconf = conf { input = T.concat [T.dropEnd 1 left, right]}
      (_, Delete) -> redrawFromCursor nconf $> proc {shellConfig = nconf}
        where
          loc = cursorLoc conf 
          inp = input conf
          right = T.reverse $ T.take loc $ T.reverse inp
          left  = T.take (T.length inp - T.length right) inp
          nconf = conf { input = T.concat [left, T.drop 1 right], cursorLoc = max 0 $ loc - 1}
      (_, Character "\NUL") -> pure proc
      (KeyModifiers 0, Character rawKey) -> do
        putStr $ T.unpack rawKey
        hFlush stdout
        redrawFromCursor conf
        pure $ proc {shellConfig = addToInput conf rawKey}
      _ -> do 
        let bind = filter (\x -> fst x == key) $ binds conf
        case bind of
          (x:_) -> snd x proc
          _ -> pure proc
  (b', p) <- chainEventHook out postHook key
  pure $ updateWithKey key p
    where
    addToInput c t = c {input = T.concat [left, t, right]}
      where
        loc = cursorLoc c 
        inp = input c
        right = T.reverse $ T.take loc $ T.reverse inp
        left  = T.take (T.length inp - T.length right) inp
    
    executablelist' :: ShellProcess -> [T.Text]
    executablelist' p = maybe [] (fromMaybe [] . fromDynamic) (lookupCache (shellCache p) "executables" >>= \x -> lookupCache x "execs")

