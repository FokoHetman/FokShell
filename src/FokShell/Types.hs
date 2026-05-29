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
import System.Posix (setTerminalProcessGroupID, stdInput, Fd, setProcessGroupIDOf, getProcessGroupIDOf)
import Control.Monad (when)

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
  tty :: Fd
, task :: Task
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

handleProcessException :: ShellConfig -> IOError -> IO Process
handleProcessException proc' e = do
  traceIO $ fromMaybe "" ((<>": ") <$> ioeGetFileName e) <> ioeGetErrorString e
  let ecode = case ioeGetErrorType e of
            NoSuchThing      -> ExitFailure 127
            PermissionDenied -> ExitFailure 126
            _                -> ExitFailure 1
  ecode' <- async $ pure ecode
  pure Process
    { exitCode = ecode'
    , pid = Nothing
    , procHandle = Nothing
    , procOuth = Nothing
    , procErrh = Nothing
    , procInh = Nothing
    }
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
