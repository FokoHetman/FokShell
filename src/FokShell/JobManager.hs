{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module FokShell.JobManager where
import FokShell.Types
import Data.Text qualified as T
import Data.Functor
import Data.Bool (bool)
import Language.Parser
import Data.List (singleton)
import Control.Concurrent.STM
import Control.Concurrent.Async (wait, async, Async)
import Control.Monad (when)
import GHC.IO.Exception (ExitCode)
import FokShell.Utils (bool')
import Debug.Trace (traceShow)
spawnJob :: Job -> TVar ShellConfig -> IO (Async ExitCode)
spawnJob job conf = spawnTask conf job.task (job.inh, job.outh, job.errh) job.processes

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

spawnTask :: TVar ShellConfig -> Task -> (TaskPipeType, TaskPipeType, TaskPipeType) -> TVar [Process] -> IO (Async ExitCode)
spawnTask conf t handles processes = case t.prevTask of
  Nothing -> do
    proc <- executeTask conf t
    atomically . modifyTVar processes $ (proc:)
    pure proc.exitCode
  Just t' -> do
    waiter <- spawnTask conf (replaceTerminalPipes t' handles) handles processes
    case t.condition of
      Just x -> async $ do
        code <- wait waiter
        bool' (x code)
          (pure code)
          (step >>= wait)
      Nothing -> step
    where
      step = do
        process' <- executeTask conf t
        atomically $ modifyTVar processes $ (process':)
        pure process'.exitCode
