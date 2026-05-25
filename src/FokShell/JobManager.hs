{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module FokShell.JobManager where
import FokShell.Types

import Data.Functor
import Data.Bool (bool)
import Language.Parser
import Data.List (singleton)
import Control.Concurrent.STM
import Control.Concurrent.Async (wait)
spawnJob :: Job -> TVar ShellConfig -> IO Job
spawnJob job conf = do
  processes <- spawnTask conf job.task (job.inh, job.outh, job.errh)
  pure job {processes = processes}

replaceTerminalPipes :: Task -> (TaskPipeType, TaskPipeType, TaskPipeType) -> Task
replaceTerminalPipes t (inh, outh, errh) = t 
  { prevTask = case t.prevTask of
      Just x -> Just $ replaceTerminalPipes x (inh, outh, errh)
      Nothing -> Nothing
  , pipeIn = case t.pipeIn of
      Terminal -> inh
      _ -> t.pipeIn
  , pipeOut = case t.pipeOut of
      Terminal -> outh
      _ -> t.pipeOut
  , pipeErr = case t.pipeErr of
      Terminal -> errh
      _ -> t.pipeErr
  }

spawnTask :: TVar ShellConfig -> Task -> (TaskPipeType, TaskPipeType, TaskPipeType) -> IO [Process]
spawnTask conf t handles = case t.prevTask of
  Nothing -> singleton <$> executeTask conf t
  Just t' -> do
    process <- spawnTask conf (replaceTerminalPipes t' handles) handles
    bool
      (pure process)
      (singleton <$> executeTask conf t)
      =<< (case t.condition of
        Just x -> case reverse process of
          (p:_) -> wait p.exitCode <&> x
          _ -> pure True
        Nothing -> pure True
      )
