{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module FokShell.JobManager where
import FokShell.Types
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Functor
import Language.Parser
import Data.List (singleton)
import Control.Concurrent.STM
import Control.Concurrent.Async (wait, async, Async)
import Control.Monad (when)
import GHC.IO.Exception (ExitCode)
import FokShell.Utils (bool')
import System.Posix (Fd, getProcessGroupIDOf, setTerminalProcessGroupID)
import System.Process
import Control.Concurrent (putMVar, tryReadMVar)
import GHC.IO.Handle (hFlush, hClose, hPutStr)
import Control.Exception (catch)
import System.IO (openFile)
