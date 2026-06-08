{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module FokShell.Module.JobManager where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import FokShell.JobManager
import FokShell.Types
import FokShell.Module
import FokShell.Utils
import Lib.Keys
import Language.Parser
import FokShell.Module.Preprocessor
import Control.Concurrent (newEmptyMVar, newMVar, isEmptyMVar, readMVar)
import FokShell.Module.Prompt (displayPrompt')
import Lib.Primitive
import FokShell.Module.Parser
import GHC.IO.Exception (ExitCode (ExitSuccess, ExitFailure))
import Control.Concurrent.STM
import Data.Functor

import Data.Map qualified as Map
import Control.Concurrent.Async (cancel, waitCatch)
import Data.Bool (bool)
import GHC.IO.Handle (hDuplicateTo)
import System.IO (stdin, stdout, stderr)
import Data.Maybe (fromJust)
import Control.Monad (forM_)
import Debug.Trace (trace)

import FokShell.Builtin
import System.Posix (getProcessGroupIDOf, getProcessGroupID, stdInput, setTerminalProcessGroupID, Fd, defaultFileFlags, OpenMode (ReadWrite), openFd, setProcessGroupIDOf, getProcessID, installHandler, sigTTIN, sigTSTP, sigTTOU, Handler (Ignore), fdToHandle)

import Control.Concurrent.Async (wait, async, Async)
import Control.Monad (when, forever)
import System.Process
import Control.Concurrent (putMVar, tryReadMVar, forkIO, killThread)
import GHC.IO.Handle (hFlush, hClose, hPutStr, BufferMode (NoBuffering), hSetBuffering, hGetChar, hPutChar)
import Control.Exception (catch)
import System.IO (openFile)


data JobManager = JobManager
  {
    jobs :: [Job]
  , jobCounter :: Int
  , builtins :: Map.Map T.Text Builtin
  , fd :: Fd
  }

jobattach :: Builtin
jobattach args (h_in, h_out, h_err) conf = do
  conf' <- readTVarIO conf
  jobmgr <- case requestModule @JobManager conf'.modules of
    (x:_) -> Just <$> readTVarIO x
    _ -> pure Nothing
  let findJob jobid = case jobmgr of
        Just x -> case filter (\job -> job.jobid == jobid) x.jobs of
          [x'] -> Just x'
          _ -> Nothing
        _ -> Nothing
  case h_in of
    ProcessData v -> (not <$> isEmptyMVar v) >>= bool
      ((mapM (attachToJob . fromJust . findJob . (read :: String -> Int) . T.unpack) args) <&> \case
        (x:_) -> x
        [] -> ExitFailure $ -1
      )
      (readMVar v >>= \case
        Left n -> case withTypeNode @TableExp n of
          Just (TableExp t) -> undefined
          Nothing-> undefined
        _ -> undefined
      )
    Terminal -> case args of
      (x:_) -> (attachToJob . fromJust . findJob . read . T.unpack) x $> ExitSuccess
      _ -> pure $ ExitFailure $ -1

attachToJob :: Job -> IO ExitCode
attachToJob Job{tty,inrh, outrh, errrh, exitCode} = do

  hSetBuffering stdin NoBuffering
  tid1 <- forkIO $ case inrh of
    Just proc_in -> do
      hSetBuffering proc_in NoBuffering
      forever $ do
        c <- hGetChar stdin
        hPutChar proc_in c
    Nothing -> pure ()

  tid2 <- forkIO $ case outrh of
    Just proc_out -> do
      hSetBuffering proc_out NoBuffering
      forever $ do
        c <- hGetChar proc_out
        hPutChar stdout c
    Nothing -> pure ()

  e <- case exitCode of
    Just x -> waitCatch x <&> \case
      Right e -> e
      Left _ -> ExitFailure $ -1
    Nothing -> pure $ ExitFailure $ -1

  killThread tid1
  killThread tid2

  pure e
  {-f inrh stdin
  f outrh stdout
  f errrh stderr
  case exitCode of
    Just e -> waitCatch e <&> \case
      Left _ -> ExitFailure $ 1
      Right x -> x
    Nothing -> pure . ExitFailure $ -1
  where
    f a b = case a of
      Just x -> hDuplicateTo x b
      Nothing -> pure ()-}

jobslist :: Builtin
jobslist args (h_in, h_out, h_err) conf = do
  conf' <- readTVarIO conf
  case requestModule @JobManager conf'.modules of
    (mgr:_) -> case h_out of
      Terminal -> do
        mgr' <- readTVarIO mgr
        T.putStrLn $ T.intercalate "\n" $ fmap (\x -> T.show x.jobid) mgr'.jobs
        pure ExitSuccess
      _ -> undefined
    _ -> undefined

instance Def JobManager where
  def = JobManager
    { jobs = []
    , jobCounter = 0
    , builtins = def
    , fd = 0
    }

instance Module' JobManager ShellConfig where
  initHook' tc conf = do
    fd <- openFd "/dev/tty" ReadWrite defaultFileFlags 
    pid <- getProcessID
    gpid <- getProcessGroupID
    setProcessGroupIDOf pid gpid
    setTerminalProcessGroupID fd gpid
    installHandler sigTTOU Ignore Nothing
    installHandler sigTTIN Ignore Nothing
    installHandler sigTSTP Ignore Nothing
    atomically . modifyTVar tc $ \tc' -> tc' {builtins = Map.insert "attach" jobattach $ Map.insert "jobs" jobslist tc'.builtins,fd=fd}
  exitHook' _ _ = pure ()
  resetHook' jm _ = do
    jmgr <- readTVarIO jm
    let attachedJobs = filter (\job -> job.attached) jmgr.jobs
    forM_ attachedJobs $ \job -> readTVarIO job.processes >>= mapM_ (\case
        Process{procHandle,exitCode} -> cancel exitCode >> case procHandle of
          Just x -> terminateProcess x
          Nothing -> pure ()
        BuiltinProcess{exitCode} -> cancel exitCode
        )
    atomically $ modifyTVar jm $ \jm' -> jm' {jobs=fmap (\job -> job {attached=False}) jm'.jobs}
    pure $ null attachedJobs
  preHook' jm p (KeyModifiers 0, Enter) = do
          putStrLn ""
          conf <- readTVarIO p
          let input' = T.strip $ conf.input
          parser <- case requestModule @ParserModule conf.modules of
                    (x:_) -> readTVarIO x
                    _ -> pure def
          let preprocess = connectPreprocessors parser.preprocessors
          let task = runParser parser.parser input' <&> (>>= makeTask) . preprocess conf . snd
          case task of
            Just t' -> t' >>= \t -> do
              mvar <- newEmptyMVar
              bool' t.attach
                (do
                  let make = (ProcessData <$>) . newMVar . Right
                  (in_h, inrh) <- do
                    (a,b) <- createPipe
                    make a <&> (,b)
                  (out_h, outrh) <- do
                    (a,b) <- createPipe
                    make a <&> (,b)
                  (err_h, errrh) <- do
                    (a,b) <- createPipe
                    make a <&> (,b)
                  jm' <- readTVarIO jm
                  proclist <- newTVarIO []
                  let job = (Job {
                      tty = jm'.fd 
                    , task = t
                    , attached = False
                    , jobid = jm'.jobCounter
                    , exitCode = Nothing
                    , processes = proclist
                    , inh = in_h
                    , inrh = Just inrh
                    , outh = out_h
                    , outrh = Just outrh
                    , errh = err_h
                    , errrh = Just errrh
                    })
                  atomically $ modifyTVar p $ \p' -> p' {input="", cursorLoc=0}
                  ecode <- spawnJob job p
                  let job' = (job :: Job) {exitCode = Just ecode}
                  jm' <- readTVarIO jm
                  T.putStrLn $ "[" <> T.show jm'.jobCounter <> "] Spawned a background job."
                  atomically . modifyTVar jm $ \jm' -> jm' {jobs = job':jm'.jobs, jobCounter = jm'.jobCounter + 1}
                  displayPrompt' =<< readTVarIO p
                  pure False
                )
                (do
                  shellPgid <- getProcessGroupID
                  jm' <- readTVarIO jm
                  proclist <- newTVarIO []
                  let job = (Job jm'.fd t True jm'.jobCounter Nothing proclist Terminal Terminal Terminal Nothing Nothing Nothing)
                  atomically $ modifyTVar p $ \p' -> p' {input="", cursorLoc=0}
                  a <- spawnJob job p
                  let job' = (job :: Job) {exitCode = Just a}
                  atomically . modifyTVar jm $ \jm' -> jm' {jobs = job':jm'.jobs, jobCounter = jm'.jobCounter + 1}
                  _ <- waitCatch a
                  setTerminalProcessGroupID jm'.fd shellPgid
                  displayPrompt' =<< readTVarIO p
                  pure False
                )
            Nothing -> (displayPrompt' =<< readTVarIO p) >> atomically (modifyTVar p $ \p' -> p' {input="",cursorLoc=0}) $> False
  preHook' _ _ _ = pure True
  postHook' _ _ _ = pure True



spawnJob :: Job -> TVar ShellConfig -> IO (Async ExitCode)
spawnJob job conf = spawnTask job.tty conf job.task (job.inh, job.outh, job.errh) job.processes

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

spawnTask :: Fd -> TVar ShellConfig -> Task -> (TaskPipeType, TaskPipeType, TaskPipeType) -> TVar [Process] -> IO (Async ExitCode)
spawnTask fd conf t handles processes = case t.prevTask of
  Nothing -> do
    proc <- executeTask fd conf t
    atomically . modifyTVar processes $ (proc:)
    pure proc.exitCode
  Just t' -> do
    waiter <- spawnTask fd conf (replaceTerminalPipes t' handles) handles processes
    case t.condition of
      Just x -> async $ do
        code <- wait waiter
        bool' (x code)
          (pure code)
          (step >>= wait)
      Nothing -> step
    where
      step = do
        process' <- executeTask fd conf t
        atomically $ modifyTVar processes $ (process':)
        pure process'.exitCode

executeTask :: Fd -> TVar ShellConfig -> Task -> IO Process
executeTask fd conf' t = do
  let name = t.procName
  let args = t.procArgs
  conf <- readTVarIO conf'
  builtins <- case requestModule @JobManager conf.modules of
    (x:_) -> (.builtins) <$> readTVarIO x
    _ -> pure Map.empty
  case Map.lookup name builtins of
    Just x  -> do
      action <- async $ x args (t.pipeIn, t.pipeOut, t.pipeErr) conf'
      pure BuiltinProcess
        { pid = Nothing
        , exitCode = action
        , procOuth = Nothing 
        , procErrh = Nothing
        , procInh = Nothing
        }
    Nothing -> do
      (`catch` handleProcessException conf) $ do
        outPipe <- getPipe t.pipeOut
        errPipe <- getPipe t.pipeErr
        inPipe <- getPipe t.pipeIn
        (inh, outh, errh, proch) <- createProcess (proc (T.unpack name) $ fmap T.unpack args) { std_out = outPipe, std_err = errPipe, std_in = inPipe, create_group = True }
        case t.pipeOut of
          ProcessData ref -> case outh of
            Just h -> putMVar ref $ Right h
            _ -> pure ()
          _ -> pure ()
        case t.pipeErr of
          ProcessData ref -> case errh of
            Just h -> putMVar ref $ Right h
            Nothing -> pure ()
          _ -> pure ()
        case t.pipeIn of
          ProcessData ref -> tryReadMVar ref >>= \case
              Just (Left n) -> case inh of
                Just inh' -> hPutStr inh' (T.unpack $ nodeToText n) >> hFlush inh' >> hClose inh'
                Nothing -> pure ()
              _ -> pure ()
          _ -> pure ()
        pid <- getPid proch
        when t.attach $ case pid of
          Just p -> do
            gpid <- getProcessGroupIDOf p
            setTerminalProcessGroupID fd gpid
          Nothing -> pure ()
        action <- async $ waitForProcess proch
        pure Process {pid = pid, procHandle = Just proch, exitCode = action, procInh = inh, procOuth = outh, procErrh = errh}
  where
    getPipe :: TaskPipeType -> IO StdStream
    getPipe (ProcessData ref) = tryReadMVar ref <&> \case
      Just (Right h) -> UseHandle h
      _ -> CreatePipe
    getPipe (Terminal) = pure Inherit
    getPipe (File f m) = openFile f m <&> UseHandle
