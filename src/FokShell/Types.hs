{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module FokShell.Types where

import Lib.Primitive
import qualified Data.Text as T
import Data.Dynamic (Dynamic, fromDynamic)
import Lib.Keys
import System.Process
import System.Directory (doesDirectoryExist, doesFileExist)
import Debug.Trace
import Data.Functor
import GHC.IO.Handle
import Language.Parser
import GHC.IO.Exception (ExitCode (ExitFailure), IOErrorType (NoSuchThing, PermissionDenied))
import Data.List (sort, group)
import Control.Concurrent (MVar, readMVar, putMVar, isEmptyMVar, tryReadMVar)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import System.IO (openFile, IOMode)
import FokShell.Module qualified as Module
import Control.Exception (catch)
import System.IO.Error (ioeGetErrorType, ioeGetErrorString, ioeGetFileName)
import FokShell.Module (ModuleContainer)
import Control.Concurrent.STM (readTVar, readTVarIO, TVar)

import Data.Map qualified as Map
import Control.Concurrent.Async (async, Async)

data Process = Process {
  pid       :: Maybe Pid
, procHandle:: Maybe ProcessHandle
, exitCode  :: Async ExitCode
, procOuth  :: Maybe Handle
, procErrh  :: Maybe Handle
, procInh   :: Maybe Handle
} | BuiltinProcess {
  pid       :: Maybe Pid
, exitCode  :: Async ExitCode
, procOuth  :: Maybe Handle
, procErrh  :: Maybe Handle
, procInh   :: Maybe Handle
}

data Job = Job {
  task :: Task
, attached :: Bool
, jobid :: Int
, exitCode :: Maybe (Async ExitCode)
, processes :: TVar [Process]
, outh :: TaskPipeType
, errh :: TaskPipeType
, inh  :: TaskPipeType
, outrh:: Maybe Handle
, errrh:: Maybe Handle
, inrh :: Maybe Handle
}
data State = InputOutput
data ShellProcess = ShellProcess {
    shellConfig :: ShellConfig
  , shellState  :: State
  , shellCache  :: Cache T.Text (Cache T.Text Dynamic)
}

type Action = TVar ShellConfig -> IO ()
type Builtin = [T.Text] -> (TaskPipeType, TaskPipeType, TaskPipeType) -> TVar ShellConfig -> IO ExitCode

safeGetChar :: Handle -> IO (Maybe Char)
safeGetChar h =
    (Just <$> hGetChar h)
    `catch` f
    where
      f :: IOError -> IO (Maybe Char)
      f = const $ pure Nothing

forward :: Handle -> Handle -> IO ()
forward read write = do
  char <- safeGetChar read
  case char of
    Just c -> do
      hPutChar write c
      forward read write
    Nothing -> pure ()

{-handleProcessException :: ShellProcess -> IOError -> IO (Process, ShellProcess)
handleProcessException proc' e = do
  traceIO $ fromMaybe "" ((<>": ") <$> ioeGetFileName e) <> ioeGetErrorString e
  let ecode = case ioeGetErrorType e of
            NoSuchThing      -> ExitFailure 127
            PermissionDenied -> ExitFailure 126
            _                -> ExitFailure 1
  pure (ecode, proc')-}
executeTask :: TVar ShellConfig -> Task -> IO Process
executeTask conf' t = do
  let name = t.procName
  let args = t.procArgs
  conf <- readTVarIO conf'
  let builtins = conf.builtins
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
      --(`catch` handleProcessException proc') $ do
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
        action <- async $ waitForProcess proch
        pure Process {pid = pid, procHandle = Just proch, exitCode = action, procInh = inh, procOuth = outh, procErrh = errh}
  where
    getPipe :: TaskPipeType -> IO StdStream
    getPipe (ProcessData ref) = tryReadMVar ref <&> \case
      Just (Right h) -> UseHandle h
      _ -> CreatePipe
    getPipe (Terminal) = pure Inherit
    getPipe (File f m) = openFile f m <&> UseHandle


count :: Eq a => [a] -> a -> Int
count xs find = length (filter (== find) xs)

dedup :: Ord a => [a] -> [a]
dedup = map head . group . sort

safeCheck :: (FilePath -> IO Bool) -> FilePath -> IO Bool
safeCheck f p = do
  b1 <- doesFileExist p
  b2 <- doesDirectoryExist p

  bool (pure True) (f p) (b1 || b2)


data ShellConfig = ShellConfig
  { cursorLoc   :: Int                  -- from the right, surprisingly
  , input       :: T.Text
  , binds       :: [(KeyEvent, Action)]
  , lastEvent   :: KeyEvent
  , trigger     :: KeyEvent             -- this should never be overriden globally, locally it should be overwritten with the keyevent trigger (example at ^L handling)
  , builtins    :: Map.Map T.Text Builtin
  --, completionRules :: [CompletionRule]
  , modules :: [Module.Module ShellConfig]
  }

instance ModuleContainer ShellConfig where
  getModules c = c.modules

executablelist :: ShellProcess -> [T.Text]
executablelist proc = maybe [] (fromMaybe [] . fromDynamic) (lookupCache (shellCache proc) "executables" >>= \x -> lookupCache x "execs")
