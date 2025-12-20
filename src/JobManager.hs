{-# LANGUAGE OverloadedStrings,LambdaCase #-}
module JobManager where

import qualified Data.Text as T
import ExposedTypes
import Data.Maybe (isJust)
import Data.Functor
import System.Process (CreateProcess(std_out, std_in, std_err), createProcess, proc, StdStream (UseHandle, Inherit), getPid, waitForProcess)
import GHC.IO.Exception (ExitCode(ExitSuccess))

import System.IO
import System.Directory (findExecutable)
import Lib.Config
import Language.Parser

--import System.Process (CreateProcess (std_out))

-- implementing Call:
-- create a function: call :: T.Text (prog) -> [T.Text] (args) -> PID 
-- job manage that.



handleJob :: ShellProcess -> IO ShellProcess
handleJob proc = do
  --putStrLn $ "Job Manager handling: " ++ T.unpack (input conf)
  let conf = shellConfig proc
  let task = mkTask $ T.strip $ input conf

  
  --(Just stdin, Just std_out, Just stderr, proc_handle) <- createProcess (proc "sudo" ["-iu"]) { std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe }

  case task of
    -- overriding input here
    Just t  -> displayTask t >>= print >> spawnJob (proc {shellConfig = conf { input="", cursorLoc=0 }}) (mkJob t Terminal Terminal Terminal)
    Nothing -> pure proc


-- todo: check whether executable exists (configurable) before launching it.
spawnJob :: ShellProcess -> Job -> IO ShellProcess
spawnJob proc' j = do
  let conf = shellConfig proc'
  case task j of
    Task c b n sin sout serr -> do
      execute <- c $ exitCodeToInt $ last_ec j
      evaluatedConf <- if execute then spawnJob proc' $ mkJob b sin sout serr else undefined
      case n of 
        Just n2 -> spawnJob evaluatedConf $ mkJob n2 sin sout serr -- or Terminal Terminal, idfk
        Nothing -> pure evaluatedConf
    PCall n a  -> do
      --if isBuiltin n then executeBuiltin n a else
      stdoutr <- getHandle pipeOut
      stdinr  <- getHandle pipeIn
      stderrr <- getHandle pipeErr

      pname' <- complexToText n
      let pname = T.unpack pname'
      args  <- mapM complexToText a
      case lookup pname' (builtins conf) of
        Just x -> x args proc'
        Nothing -> do
          exists <- findExecutable pname <&> isJust
          if not exists then putStrLn (pname ++ ": command not found") $> proc' {- TODO: once exit statuses arive, return -1 here. -} else do
            (s_in,sout,serr,ph) <- createProcess (proc pname $ fmap T.unpack args)
                {std_out = stdoutr, std_in = stdinr, std_err = stderrr}
            let (JobMgr jobs) = jobManager conf
            let newjob = j {stdinj = s_in, stdoutj = sout, stderrj = serr}

            -- use process handle instead of pid in Job.
            getPid ph >>= \case
              Just p -> do
                exitcode <- waitForProcess ph
                pure proc' {shellConfig = conf {jobManager = JobMgr $ (newjob {pid = Just p, last_ec = exitcode}):jobs}}
              Nothing -> undefined -- undefined behavior. idk what to do when process has no id
  --(t', h) <- walkTask $ task j
  where
    getHandle :: (Job -> PipeType) -> IO StdStream
    getHandle fun = case fun j of
        Terminal -> pure Inherit
        File f m -> f >>= \uf -> openFile (T.unpack uf) m >>= \x -> pure $ UseHandle x


--walkTask :: Task -> IO (Task, Bool)
--walkTask (PCall e a) = Nothing

mkJob :: Task -> PipeType -> PipeType -> PipeType -> Job
mkJob t i o e = Job {
    pid       = Nothing
  , task      = t
  , stdinj    = Nothing
  , stdoutj   = Nothing
  , stderrj   = Nothing
  , last_ec   = ExitSuccess

  , pipeIn    = i
  , pipeOut   = o
  , pipeErr   = e
  }

mkTask :: T.Text -> Maybe Task
mkTask t = do
  (_, n) <- runParser parseExpr t
  taskify n



