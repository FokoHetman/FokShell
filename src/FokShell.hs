{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module FokShell where

import InputHandling (nextEvent, KeyEvent, KeyCode (Character, Escape), KeyModifiers (KeyModifiers), control)

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



-- TODO:
-- handle printing prompt with input, cursor, etc
-- job mgr


class Def a where
  def :: a

type Hook = ShellProcess -> IO Bool -- bool tells whether to continue afterhand action

type Action = ShellProcess -> IO ShellProcess


data ShellHooks = ShellHooks 
  { haltHook  :: Hook -- things to do before a HALT (^C)
  , exitHook  :: Hook -- things to do before exitting (^D, exit, etc)
  , startHook :: Hook -- like rc
  , clearHook :: Hook
  }


defaultHaltHook :: Hook 
defaultHaltHook _ = do
  putStrLn "^C"
  pure True

defaultExitHook :: Hook
defaultExitHook _ = do
  putStrLn "\nexit"
  pure True

defaultClearHook :: Hook
defaultClearHook (ShellProcess conf _) = do
  if lastEvent conf == trigger conf then
    pure False
  else
    pure True

defaultStartHook :: Hook
defaultStartHook _ = pure True

instance Def ShellHooks where
  def = ShellHooks
    { haltHook  = defaultHaltHook
    , exitHook  = defaultExitHook
    , startHook = defaultStartHook
    , clearHook = defaultClearHook
    }

type PromptText = T.Text
data Swallow = Never | Swallowed PromptText
data Prompt  = SingleLine PromptText Swallow | MultiLine PromptText Swallow


getFormattedDirectory :: IO T.Text
getFormattedDirectory = do
  dir <- getCurrentDirectory
  home <- getHomeDirectory
  pure $ T.replace (T.pack home) "~" (T.pack dir)

shortcuts :: [(T.Text, IO T.Text)]
shortcuts = [("%u", T.pack <$> getEffectiveUserName), ("%d", getFormattedDirectory)]

replaceShortcuts :: [(T.Text, IO T.Text)] -> IO T.Text -> IO T.Text
replaceShortcuts (x:xs) text = do
  t <- text
  replaceShortcuts xs ((\y -> T.replace (fst x) y t) <$> snd x)
replaceShortcuts [] text = text
displayPrompt :: Prompt -> IO ()
displayPrompt = \case 
  SingleLine text _ -> eputStrf <$> formatted $ pure text
  MultiLine text _  -> eputStrf <$> formatted $ pure text
  where
    formatted :: IO T.Text -> IO T.Text
    formatted = replaceShortcuts shortcuts

data ShellConfig = ShellConfig
  { hooks     :: ShellHooks
  , prompt    :: Prompt
  , input     :: T.Text
  , binds     :: [(KeyEvent, Action)]
  , lastEvent :: KeyEvent
  , trigger   :: KeyEvent             -- this should never be overriden globally, locally it should be overwritten with the keyevent trigger (example at ^L handling)
  }

data State = InputOutput

data ShellProcess = ShellProcess ShellConfig State


instance Def ShellConfig where
  def = ShellConfig
    { hooks = def
    , prompt = SingleLine "%d > " Never
    , input = ""
    , binds = def
    , lastEvent = (KeyModifiers 0, Escape)
    , trigger = (KeyModifiers 0, Escape)
    }

haltAction :: Action
haltAction (ShellProcess config state) = displayPrompt (prompt config) $> ShellProcess (config {input = ""}) state

exitAction :: Action
exitAction (ShellProcess _ _) = exitSuccess

clearAction :: Action
clearAction (ShellProcess c s) = putStrLn "\ESC[2J\ESC[H" *> displayPrompt (prompt c) $> ShellProcess c s


instance Def [(KeyEvent, Action)] where
  def = [
        ((control, Character "c"), \(ShellProcess config state) -> haltHook (hooks config) (ShellProcess config state) >>= \x -> if x then haltAction (ShellProcess config state) else pure $ ShellProcess config state)
      , ((control, Character "d"), \(ShellProcess config state) -> exitHook (hooks config) (ShellProcess config state) >>= \x -> if x then exitAction (ShellProcess config state) else pure $ ShellProcess config state)
      , ((control, Character "l"), \(ShellProcess config state) -> clearHook(hooks config) (ShellProcess config {trigger=(control, Character "l")} state) >>= \x -> if x then clearAction (ShellProcess config state) else pure $ ShellProcess config state)
    ]


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




eputStrf :: IO T.Text -> IO ()
eputStrf t = t >>= \x -> putStr (T.unpack x) <> hFlush stdout

putStrf :: T.Text -> IO ()
putStrf t = putStr (T.unpack t) <> hFlush stdout


updateWithKey :: KeyEvent -> ShellProcess -> ShellProcess
updateWithKey event (ShellProcess conf state) = ShellProcess conf {lastEvent=event} state

parseEvent :: ShellProcess -> KeyEvent -> IO ShellProcess 
parseEvent (ShellProcess conf state) key = do 
  out <- case key of 
    (KeyModifiers 0, Character rawKey) -> do
      putStr $ T.unpack rawKey
      hFlush stdout
      pure $ ShellProcess (addToInput conf rawKey) state
    _ -> do 
      let bind = filter (\x -> fst x == key) $ binds conf
      unwrapBind bind $ ShellProcess conf state
  pure $ updateWithKey key out
  where
    unwrapBind [x] defval = snd x defval
    unwrapBind [] defval = pure defval
    unwrapBind _ _ = undefined
    addToInput c t = conf {input = T.concat [input c, t]}


-- reimplement the good input handling
--process :: ShellConfig -> IO ()
--process config = do
--  getChar >>= \case
--    '\x03' -> do
--      _ <- haltHook $ hooks config
--      process config
--    '\x04' -> do
--      pure ()
--    --'\ESC' -> 
--    x -> pure ()
