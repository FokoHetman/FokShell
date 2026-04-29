{-# LANGUAGE OverloadedStrings #-}
module FokShell.JobManager where
import Lib.Config
import Data.Text qualified as T

import GHC.IO.Exception (ExitCode)
import Data.Bool (bool)

handleJob :: ShellProcess -> IO (Maybe Job, ShellProcess)
handleJob proc = do
  let conf = shellConfig proc
  let task = mkTask' $ T.strip $ input conf

  case task of
    Just t  -> t >>= \t -> do
      let job = Job t
      p <- spawnJob (proc {shellConfig = conf { input="", cursorLoc=0 }}) job
      pure (Just job, p)
    Nothing -> pure (Nothing, proc {shellConfig = conf {input="",cursorLoc=0}})

spawnJob :: ShellProcess -> Job -> IO (ExitCode, ShellProcess)
spawnJob proc job = spawnTask proc job.task

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
