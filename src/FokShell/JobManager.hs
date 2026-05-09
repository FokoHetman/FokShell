{-# LANGUAGE OverloadedStrings #-}
module FokShell.JobManager where
import FokShell.Types
import FokShell.Utils
import Data.Text qualified as T

import GHC.IO.Exception (ExitCode)
import Data.Bool (bool)
import Control.Concurrent (putMVar, newEmptyMVar)
import Language.Parser
import FokShell.Module.Parser
import FokShell.Module (requestModule)
import Lib.Primitive (Def(def))
import Data.Data (Proxy(Proxy))

handleJob :: ShellProcess -> IO (Maybe Job, ShellProcess)
handleJob proc = do
  let conf = shellConfig proc
  let parser = case requestModule (Proxy @ParserModule) conf.modules of
        (x:_) -> x
        _ -> def
  let task = makeTask . snd <$> runParser parser.parser (T.strip $ input conf)

  case task of
    Just t'  -> t' >>= \t -> do
      mvar <- newEmptyMVar
      let job = Job t mvar
      p <- spawnJob (proc {shellConfig = conf { input="", cursorLoc=0 }}) job
      pure (Just job, p)
    Nothing -> pure (Nothing, proc {shellConfig = conf {input="",cursorLoc=0}})

spawnJob :: ShellProcess -> Job -> IO ShellProcess
spawnJob proc job = do
  (exitCode, proc') <- spawnTask proc job.task
  putMVar job.exitCode exitCode
  pure proc'

spawnTask :: ShellProcess -> Task -> IO (ExitCode, ShellProcess)
spawnTask proc t = case t.prevTask of
  Nothing -> executeTask proc t
  Just x -> do
    (exitCode, nproc) <- spawnTask proc x
    bool
      (pure (exitCode, nproc))
      (executeTask nproc t)
      (t.condition exitCode)
