{-# LANGUAGE OverloadedStrings #-}
module FokShell.JobManager where
import FokShell.Types
import FokShell.Utils
import Data.Text qualified as T

import GHC.IO.Exception (ExitCode)
import Data.Bool (bool)
import Control.Concurrent (putMVar, newEmptyMVar)
import Language.Parser
import Control.Arrow (Arrow(first))
import Data.List (singleton)

spawnJob :: ShellProcess -> Job -> IO ShellProcess
spawnJob proc job = do
  (exitCode, proc') <- spawnTask proc job.task
  putMVar job.exitCode exitCode
  pure proc'

spawnTask :: ShellProcess -> Task -> IO ([Process], ShellProcess)
spawnTask proc t = case t.prevTask of
  Nothing -> first singleton <$> executeTask proc t
  Just x -> do
    (process, nproc) <- spawnTask proc x
    bool
      (pure (process, nproc))
      (first singleton <$> executeTask nproc t)
      (t.condition exitCode)
