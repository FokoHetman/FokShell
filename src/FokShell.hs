{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module FokShell where

import InputHandling (nextEvent, KeyEvent, KeyCode (Character), KeyModifiers (KeyModifiers), control)

import qualified Data.Text as T
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (when, unless)
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode (NoBuffering), hFlush, stdout)
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Posix (getEffectiveUserName)

class Def a where
  def :: a

type Hook = IO Bool -- bool tells whether to continue afterhand action

type Action = ShellProcess -> IO ShellProcess


data ShellHooks = ShellHooks 
  { haltHook  :: Hook -- things to do before a HALT (^C)
  , exitHook  :: Hook -- things to do before exitting (^D, exit, etc)
  , startHook :: Hook -- like rc
  }


defaultHaltHook :: Hook 
defaultHaltHook = do
  putStrLn "\n"
  pure True

defaultExitHook :: Hook
defaultExitHook = do
  putStrLn "\nexit"
  pure True

defaultStartHook :: Hook
defaultStartHook = pure True

instance Def ShellHooks where
  def = ShellHooks
    { haltHook  = defaultHaltHook
    , exitHook  = defaultExitHook
    , startHook = defaultStartHook
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
  { hooks  :: ShellHooks
  , prompt :: Prompt
  , input  :: T.Text
  , binds  :: [(KeyEvent, Action)]
  }

data State = InputOutput

data ShellProcess = ShellProcess ShellConfig State


instance Def ShellConfig where
  def = ShellConfig
    { hooks = def
    , prompt = SingleLine "%d > " Never
    , input = ""
    , binds = def
    }

instance Def [(KeyEvent, Action)] where
  def = [
      ((control, Character "h"), \x -> putStrLn "\nhi!" >> pure x)
    ]


fokshell :: ShellConfig -> IO ()
fokshell config = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  
  doStart <- startHook $ hooks config
  unless doStart exitSuccess

  -- todo: event loop
  displayPrompt $ prompt config
  eventLoop $ ShellProcess config InputOutput


eventLoop :: ShellProcess -> IO ()
eventLoop proc = do
  
  -- implement reversion (event -> string), or also pass the raw string here.
  -- this will help with a job handler.
  event <- nextEvent
  parseEvent proc event >>= eventLoop

eputStrf :: IO T.Text -> IO ()
eputStrf t = do
  t >>= \x -> putStr (T.unpack x) <> hFlush stdout

putStrf :: T.Text -> IO ()
putStrf t = putStr (T.unpack t) <> hFlush stdout


parseEvent :: ShellProcess -> KeyEvent -> IO ShellProcess 
parseEvent (ShellProcess conf state) key = case key of 
    (KeyModifiers 0, Character rawKey) -> do
      putStr $ T.unpack rawKey
      hFlush stdout
      pure $ ShellProcess (addToInput conf rawKey) state
    _ -> do 
      let bind = filter (\x -> fst x == key) $ binds conf
      unwrapBind bind $ ShellProcess conf state
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
