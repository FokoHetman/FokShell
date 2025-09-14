{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module FokShell where

import InputHandling (nextEvent)

import Data.Text as T
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (when, unless)
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode (NoBuffering))

class Def a where
  def :: a

type Hook = IO Bool -- bool tells whether to continue afterhand action

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
data Swallow = Never | Old PromptText
data Prompt  = SingleLine PromptText | MultiLine PromptText Swallow

data ShellConfig = ShellConfig
  { hooks  :: ShellHooks
  , prompt :: Prompt
  }

data State = InputOutput

data ShellProcess = Process ShellConfig State


instance Def ShellConfig where
  def = ShellConfig
    { hooks = def
    , prompt = SingleLine "%d > "
    }

fokshell :: ShellConfig -> IO ()
fokshell config = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  
  doStart <- startHook $ hooks config
  unless doStart exitSuccess

  -- todo: event loop

  event <- nextEvent
  print event
  --process config
  --exitLoop config
--exitLoop :: ShellConfig -> IO ()
--exitLoop config = do
--  doExit <- exitHook $ hooks config
--  when doExit exitSuccess
--  process config 
--  exitLoop config


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
