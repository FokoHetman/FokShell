{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module FokShell.Types where

import Lib.Primitive
import qualified Data.Text as T
import Data.Dynamic (Dynamic, fromDynamic)
import Lib.Keys
import System.Process
import System.Directory (doesDirectoryExist, doesFileExist)
import Debug.Trace
import GHC.IO.Handle
import Language.Parser
import GHC.IO.Exception (ExitCode (ExitFailure), IOErrorType (NoSuchThing, PermissionDenied))
import Data.List (sort, group)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import FokShell.Module qualified as Module
import Control.Exception (catch)
import System.IO.Error (ioeGetErrorType, ioeGetErrorString, ioeGetFileName)
import Control.Concurrent.STM (TVar)

import Data.Map qualified as Map
import Control.Concurrent.Async (async, Async)
import System.Posix (Fd)
import Control.Monad.IO.Class (MonadIO)

-- DefaultIO class {{{
class DefaultIO a where defM :: MonadIO m => m a
-- }}}

-- process datatypes {{{
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
-- }}}

data ShellProcess = ShellProcess {
    shellConfig :: ShellConfig
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
forward readh writeh = do
  char <- safeGetChar readh
  case char of
    Just c -> do
      hPutChar writeh c
      forward readh writeh
    Nothing -> pure ()

handleProcessException :: ShellConfig -> IOError -> IO Process
handleProcessException _proc e = do
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

instance Module.ModuleContainer ShellConfig where
  getModules c = c.modules

executablelist :: ShellProcess -> [T.Text]
executablelist shellproc = maybe [] (fromMaybe [] . fromDynamic) (lookupCache (shellCache shellproc) "executables" >>= \x -> lookupCache x "execs")

-- vim: foldmethod=marker
