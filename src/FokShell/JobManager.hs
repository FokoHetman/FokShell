{-# LANGUAGE OverloadedStrings #-}
module FokShell.JobManager where
import Lib.Config
import Data.Text qualified as T

import GHC.IO.Exception (ExitCode)
import Data.Bool (bool)

handleJob :: ShellProcess -> IO ShellProcess
handleJob proc = do
  let conf = shellConfig proc
  let task = mkTask' $ T.strip $ input conf

  case task of
    Just t  -> t >>= {-displayTask t >>= print >> -} spawnJob (proc {shellConfig = conf { input="", cursorLoc=0 }}) . Job
    Nothing -> pure proc {shellConfig = conf {input="",cursorLoc=0}}

spawnJob :: ShellProcess -> Job -> IO ShellProcess
spawnJob proc job = do
  -- TODO: append `job` to process' jobs, handle job stuff
  finExitCode <- spawnTask proc job.task
  pure proc

spawnTask :: ShellProcess -> Task -> IO (ExitCode, ShellProcess)
spawnTask proc t = case t.prevTask of
  Nothing -> executeTask proc t
  Just x -> do
    (exitCode, nproc) <- spawnTask proc x
    bool
      (pure (exitCode, nproc))
      (executeTask nproc t)
      (t.condition exitCode)

-- TODO: on | make A.stdout and B.stdin an IORef of NodePipe
