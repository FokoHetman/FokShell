{-# LANGUAGE OverloadedStrings, LambdaCase, ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

module FokShell.Types where

import Lib.Primitive
import qualified Data.Text as T
import Lib.Keys
import System.Process
import GHC.IO.Handle
import GHC.IO.Exception (ExitCode, IOException)
import FokShell.Module qualified as Module
import Control.Exception (catch, throwIO, try, finally)
import Control.Concurrent.STM (TVar, readTVarIO, readTBQueue, writeTBQueue, atomically, newTQueueIO, writeTQueue, readTQueue, TMVar)

import Control.Concurrent.Async (Async, withAsync, link, cancel)
import System.Posix (Fd, setTerminalProcessGroupID, getProcessGroupIDOf, ProcessGroupID)
import Control.Monad.Reader (ReaderT (runReaderT, ReaderT), asks, MonadIO (liftIO), MonadReader (ask))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Concurrent (Chan)
import qualified Data.ByteString as BS
import GHC.Natural (Natural)
import Control.Monad (void)
import System.IO.Error (isResourceVanishedError, catchIOError)
import Prettyprinter
import Type.Reflection
import FokShell.ShellNode


data ShellError = PipelineTypeError Span T.Text | SinkMismatch | SourceMismatch | ModuleNotFoundError | ErrorIO IOError | Unknown String deriving (Eq,Show)
shellError :: ShellError -> IO ()
shellError = putStrLn . show


catchIO :: ShellAction a -> ShellAction a
catchIO f = ExceptT $ ReaderT $ \env -> runReaderT (runExceptT f) env `catchIOError` \e -> runReaderT (runExceptT (throwError (ErrorIO e) :: ShellAction a)) env

catchIO' :: IO a -> ShellAction a
catchIO' f = ExceptT $ ReaderT $ \env -> (Right <$> f) `catchIOError` \e -> runReaderT (runExceptT (throwError (ErrorIO e) :: ShellAction a)) env


data Span = Span Int Int deriving (Eq,Ord,Show)

data Pipeline i o where
  Stage  :: (Typeable i, Typeable o, PrettyAnn ShellAnn o) => Process i o -> [T.Text] -> Pipeline i o
  Adapter :: (Typeable i, Typeable o1, PrettyAnn ShellAnn o, PrettyAnn ShellAnn o1) => (o1 -> ShellAction o) -> Pipeline i o1 -> Pipeline i o
  Then   :: (Typeable i, Typeable mid, Typeable o, PrettyAnn ShellAnn mid, PrettyAnn ShellAnn o) => Pipeline i mid -> Pipeline mid o -> Pipeline i o
  Followed :: (Typeable i, Typeable o, PrettyAnn ShellAnn o) => Pipeline i o -> Pipeline i o -> (ExitCode -> ShellAction Bool) -> Pipeline i o

type Process input output = [T.Text] -> Source input -> Sink output -> ShellAction ExitCode

infixr 1 >->
(>->) :: (Typeable i, Typeable mid, Typeable o, PrettyAnn ShellAnn mid, PrettyAnn ShellAnn o) => Pipeline i mid -> Pipeline mid o -> Pipeline i o
(>->) = Then

newTypedPipe :: MonadIO m => m (Source a, Sink a)
newTypedPipe = do
  queue <- liftIO newTQueueIO
  let source = Source {pull = atomically (readTQueue queue), rawOverride = Nothing}
      sink = Sink
        { push = atomically . writeTQueue queue . Just
        , finish = atomically $ writeTQueue queue Nothing
        , rawOverride = Nothing
        }
  pure (source, sink)

data SomeProcess where
  SomeProcess :: (Typeable i, Typeable o, PrettyAnn ShellAnn o) => Process i o -> SomeProcess
data SomePipeline where
  SomePipeline :: (Typeable i, Typeable o, PrettyAnn ShellAnn o) => Pipeline i o -> SomePipeline

data Source a = Source
  { pull :: IO (Maybe a)
  , rawOverride :: Maybe StdStream
  }

data SomeSource where SomeSource :: Typeable a => Source a -> SomeSource

data Sink a = Sink
  { push   :: a -> IO ()
  , finish :: IO ()
  , rawOverride :: Maybe StdStream
  }

data SomeSink where SomeSink :: Typeable a => Sink a -> SomeSink

data Job = Job
  { tty     :: Fd
  , jobId   :: Int
  , pipeline:: SomePipeline
  , processes :: TVar [SomeProcess]
  }
data SpawnedJob = SpawnedJob Job (Async (Either ShellError ExitCode))

instance Module.ModuleError ShellError where
  moduleNotFoundError = ModuleNotFoundError

data ShellEnv = ShellEnv
  { envModules  :: TVar [Module.Module ShellAction]
  , eventChan   :: Chan KeyEvent
  , ttyFd       :: Fd
  , ttyHandle   :: Handle
  , ttyLock     :: TMVar ()
  , pgid        :: ProcessGroupID
  }

type Shell = ReaderT ShellEnv IO
type ShellAction = ExceptT ShellError Shell

instance Module.ModuleContainer ShellAction where
  getModules = liftIO . readTVarIO =<< asks envModules

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

data FokShell = FokShell
  { modules     :: [Module.ModuleConfig ShellAction]
  }

instance Def FokShell where
  def = FokShell []

-- vim: foldmethod=marker
