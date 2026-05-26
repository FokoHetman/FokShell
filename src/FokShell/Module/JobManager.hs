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
import Data.Data (Proxy(Proxy))
import System.Process (createPipe, waitForProcess, terminateProcess)
import GHC.IO.Exception (ExitCode (ExitSuccess, ExitFailure))
import Control.Concurrent.STM
import Data.Functor

import Data.Map qualified as Map
import Control.Concurrent.Async (poll, wait, cancel, waitCatch)
import Data.Bool (bool)
import GHC.IO.Handle (hDuplicateTo, hFlush)
import System.IO (stdin, stdout, stderr)
import Data.Maybe (fromJust)
import Control.Monad (forM_)
import System.Posix (signalProcess, sigTERM)
import Debug.Trace (trace)

data JobManager = JobManager
  {
    jobs :: [Job]
  , jobCounter :: Int
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
      (mapM_ (attachToJob . fromJust . findJob . (read :: String -> Int) . T.unpack) args $> ExitSuccess)
      (readMVar v >>= \case
        Left n -> case withTypeNode @TableExp n of
          Just (TableExp t) -> undefined
          Nothing-> undefined
        _ -> undefined
      )
    Terminal -> case args of
      (x:_) -> (attachToJob . fromJust . findJob . read . T.unpack) x $> ExitSuccess
      _ -> pure $ ExitFailure $ -1

attachToJob :: Job -> IO ()
attachToJob Job{inrh, outrh, errrh} = do
  f inrh stdin
  f outrh stdout
  f errrh stderr
  where
    f a b = case a of
      Just x -> hDuplicateTo x b
      Nothing -> pure ()

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
    }

instance Module' JobManager ShellConfig where
  initHook' _ conf = atomically . modifyTVar conf $ \c -> c {builtins = Map.insert "attach" jobattach $ Map.insert "jobs" jobslist c.builtins}
  exitHook' _ _ = pure ()
  resetHook' jm _ = do
    print "achtung"
    jmgr <- readTVarIO jm
    print $ fmap (\job -> job.attached) jmgr.jobs
    print jmgr.jobCounter
    forM_ (filter (\job -> job.attached) jmgr.jobs) $ \job -> mapM_ (\case
      Process{procHandle,exitCode} -> cancel exitCode >> case procHandle of
        Just x -> trace "terminated" $ terminateProcess x
        Nothing -> trace "?" $ pure ()
      BuiltinProcess{exitCode} -> trace "??" $ cancel exitCode
      ) job.processes
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
                  let job = (Job {
                      task = t
                    , attached = False
                    , jobid = jm'.jobCounter
                    , exitCode = mvar
                    , processes = []
                    , inh = in_h
                    , inrh = Just inrh
                    , outh = out_h
                    , outrh = Just outrh
                    , errh = err_h
                    , errrh = Just errrh
                    })
                  atomically $ modifyTVar p $ \p' -> p' {input="", cursorLoc=0}
                  job' <- spawnJob job p
                  jm' <- readTVarIO jm
                  T.putStrLn $ "[" <> T.show jm'.jobCounter <> "] Spawned a background job."
                  atomically . modifyTVar jm $ \jm' -> jm' {jobs = job':jm'.jobs, jobCounter = jm'.jobCounter + 1}
                  print "append"
                  displayPrompt' =<< readTVarIO p
                  pure False
                )
                (do
                  jm' <- readTVarIO jm
                  let job = (Job t True jm'.jobCounter mvar [] Terminal Terminal Terminal Nothing Nothing Nothing)
                  atomically $ modifyTVar p $ \p' -> p' {input="", cursorLoc=0}
                  job' <- spawnJob job p
                  atomically . modifyTVar jm $ \jm' -> jm' {jobs = job':jm'.jobs, jobCounter = jm'.jobCounter + 1}
                  case reverse job'.processes of
                    (x:_) -> waitCatch x.exitCode >> pure ()
                    _ -> pure ()
                  displayPrompt' =<< readTVarIO p
                  pure False
                )
            Nothing -> (displayPrompt' =<< readTVarIO p) >> atomically (modifyTVar p $ \p' -> p' {input="",cursorLoc=0}) $> False
  preHook' _ _ _ = pure True
  postHook' _ _ _ = pure True
