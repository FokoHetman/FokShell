{-# LANGUAGE OverloadedStrings, LambdaCase, AllowAmbiguousTypes, ExplicitNamespaces, GADTs #-}
module FokShell.Module.JobManager where

import Data.Void
import Data.Text qualified as T
import FokShell.Types
import FokShell.Module
import FokShell.Utils
import Lib.Keys
import FokShell.Module.Prompt (displayPrompt')
import Lib.Primitive
import FokShell.Module.Parser
import GHC.IO.Exception (ExitCode (ExitSuccess, ExitFailure))
import Control.Concurrent.STM
import Data.Functor

import Data.Map qualified as Map
import Control.Concurrent.Async (cancel, waitCatch)
import Data.Bool (bool)
import System.Posix (getProcessGroupID, setTerminalProcessGroupID, setProcessGroupIDOf, getProcessID, ProcessID, ProcessGroupID)

import Control.Concurrent.Async (async, waitEitherCatch, link, waitBoth)
import Control.Exception (Exception (displayException))
import Control.Monad.IO.Class (MonadIO(liftIO))
import FokShell.Module.Input
import Text.Megaparsec (errorBundlePretty, runParser)
import qualified Data.Map as M
import Control.Monad.RWS (MonadReader(ask))
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.Except (runExceptT, MonadError (throwError, catchError))
import qualified Data.ByteString.Char8 as BS
import Data.Data (eqT, type (:~:) (Refl), Typeable)
import Prettyprinter
import Prettyprinter.Render.Terminal (renderIO)
import FokShell.ShellNode
import System.Directory.Internal.Prelude (ioeGetErrorString, ioeGetLocation, ioeGetErrorType)
import System.IO.Error (ioeGetFileName)
import System.Console.Terminal.Size qualified as TS
import System.Process (StdStream(Inherit))


data JobManagerConfig = JobManagerConfig
  { builtins :: Map.Map T.Text SomeProcess
  }

instance Def JobManagerConfig where
  def = JobManagerConfig Map.empty

data JobManager = JobManager
  { jobs :: TVar (M.Map Int SpawnedJob)
  , attachedJob :: TMVar Int
  , jobCounter :: TVar Int
  , builtins :: TVar (Map.Map T.Text SomeProcess)
  , pid :: ProcessID
  , gpid :: ProcessGroupID
  , lastExitCode :: TVar ExitCode
  }


instance Configurable JobManager JobManagerConfig where
  configure jm = liftIO $ do
    pid <- getProcessID 
    gpid <- getProcessGroupID 
    atomically $ do
      jobs <- newTVar M.empty
      attachedJob <- newEmptyTMVar
      jobCounter <- newTVar 0
      builtins <- newTVar jm.builtins
      lastExitCode <- newTVar ExitSuccess
      pure $ JobManager
        { jobs
        , attachedJob
        , jobCounter
        , builtins
        , pid
        , gpid
        , lastExitCode
        }
instance Module' JobManager ShellAction where
  initHook jm = do
    env <- ask
    liftIO $ setProcessGroupIDOf jm.pid jm.gpid
    liftIO $ setTerminalProcessGroupID env.ttyFd jm.gpid
  exitHook jm = do
    env <- ask
    liftIO $ setProcessGroupIDOf jm.pid jm.gpid
    liftIO $ setTerminalProcessGroupID env.ttyFd jm.gpid
  resetHook jmgr = do
    liftIO (atomically $ tryReadTMVar jmgr.attachedJob) >>= \case
      Just x -> liftIO (M.lookup x <$> readTVarIO jmgr.jobs) >>= \case
        Just job -> undefined {-terminateJob-} job $> False
        Nothing -> pure True
      Nothing -> pure True
  preHook jm (KeyModifiers 0, Enter) = do
    liftIO $ putStrLn ""
    inputModule <- get @Input
    input <- liftIO $ T.strip <$> readTVarIO inputModule.input
    parser <- (.parser) <$> get @ParserModule
    liftIO . atomically $ do
      writeTVar inputModule.input ""
      writeTVar inputModule.cursorLocation 0
    case (input, runParser parser "<userinput>" input) of
      ("", _) -> displayPrompt' $> False
      (_, Left e) -> do
        liftIO . putStrLn $ errorBundlePretty e
        displayPrompt'
        pure False
      (_, Right p') -> buildPipeline p' >>= \(SomePipeline pipeline) -> do
        env <- ask
        let tty = env.ttyFd
        void . liftIO . atomically $ takeTMVar env.ttyLock
        jobId <- liftIO $ readTVarIO jm.jobCounter
        processes <- liftIO $ newTVarIO []
        let job = Job
              { tty
              , pipeline = SomePipeline pipeline
              , jobId
              , processes
              }
        let source :: Source BS.ByteString
            source = Source
              { pull = do
                chunk <- BS.hGetSome env.ttyHandle 4096
                pure $
                  if BS.null chunk
                    then Nothing
                    else Just chunk
              , rawOverride = Just Inherit
              }
        termSize <- liftIO TS.size
        let w = maybe 80 TS.width termSize
        let sink :: Sink ShellNode = Sink
              { push = renderIO env.ttyHandle . layoutPretty (LayoutOptions (AvailablePerLine w 1.0)) . reAnnotate shellAnn . prettyAnn
              , finish = pure ()
              , rawOverride = Just Inherit
              }
        jobExitCode <- catchIO (spawnPipeline jm job (SomePipeline $ shellNodeAdapter pipeline) (SomeSource source) (SomeSink sink))
            `catchError`
            \case
              PipelineTypeError span e -> undefined
              SinkMismatch -> undefined
              SourceMismatch -> undefined
              ErrorIO e -> liftIO (putStrLn $ concat
                  ["IO Error: ", ioeGetErrorString e, "\nat: ", ioeGetLocation e, " ", show $ ioeGetErrorType e, " ", show $ ioeGetFileName e]) $> ExitFailure 1
              _ -> pure $ ExitFailure 1
        liftIO . atomically $ do
          putTMVar env.ttyLock ()
          writeTVar jm.lastExitCode jobExitCode
          modifyTVar jm.jobs $ M.insert jobId $ SpawnedJob job undefined --jobExitCode
          modifyTVar jm.jobCounter (+1)
        liftIO $ putStrLn ""
        displayPrompt'
        {-unless task.background $ liftIO $ do
          void $ waitCatch jobExitCode
          shellPgid <- liftIO getProcessGroupID
          liftIO $ setTerminalProcessGroupID tty shellPgid-}
        pure False
  preHook _ _ = pure True
  postHook _ _ = pure True
spawnPipeline :: JobManager -> Job -> SomePipeline -> SomeSource -> SomeSink -> ShellAction ExitCode
spawnPipeline _jm _job (SomePipeline (Stage process args :: Pipeline i o)) (SomeSource (source :: Source sourceT)) (SomeSink (sink :: Sink sinkT)) = do
  case eqT @i @sourceT of
    Just Refl -> case eqT @o @sinkT of
      Just Refl -> do
        ec <- process args source sink
        liftIO sink.finish
        pure ec
      Nothing -> throwError SinkMismatch
    Nothing -> case eqT @i @Void of
      Just Refl -> case eqT @o @sinkT of
        Just Refl -> do
          ec <- process args (Source {pull = pure Nothing, rawOverride = Nothing}) sink
          liftIO sink.finish
          pure ec
        Nothing -> throwError SinkMismatch
      Nothing -> throwError SourceMismatch

spawnPipeline jm job (SomePipeline (Adapter f (p1 :: Pipeline i o1) :: Pipeline i o)) (SomeSource (source :: Source sourceT)) (SomeSink (sink :: Sink sinkT)) = do
  case eqT @i @sourceT of
    Just Refl -> case eqT @o @sinkT of
      Just Refl -> do
        env <- ask
        (m_source, m_sink) <- newTypedPipe
        left <- liftIO . async $ runReaderT (runExceptT (catchIO $ spawnPipeline jm job (SomePipeline p1) (SomeSource source) (SomeSink m_sink {rawOverride = sink.rawOverride}))) env
        right <- liftIO . async $ runReaderT (runExceptT (catchIO $ go m_source sink)) env
        liftIO $ link left
        liftIO $ link right
        fst <$> liftIO (waitBoth left right) >>= \case
          Left e -> throwError e
          Right x-> pure x
      Nothing -> throwError SinkMismatch
    Nothing -> case eqT @i @Void of
      Just Refl -> case eqT @o @sinkT of
        Just Refl -> do
          env <- ask
          (m_source, m_sink) <- newTypedPipe
          left <- liftIO . async $ runReaderT (runExceptT (catchIO $ spawnPipeline jm job (SomePipeline p1) (SomeSource source) (SomeSink m_sink))) env
          right <- liftIO . async $ runReaderT (runExceptT (catchIO $ go m_source sink)) env
          liftIO $ link left
          liftIO $ link right
          fst <$> liftIO (waitBoth left right) >>= \case
            Left e -> throwError e
            Right x-> pure x
        Nothing -> throwError SinkMismatch
      Nothing -> throwError SourceMismatch
  where
  go :: Source o1 -> Sink o -> ShellAction ()
  go src out = liftIO src.pull >>= \case
    Just x -> (f x) >>= liftIO . out.push >> go src out
    Nothing-> liftIO out.finish
spawnPipeline jm job (SomePipeline (Then (p1 :: Pipeline i m) (p2 :: Pipeline m o))) (SomeSource (source :: Source sourceT)) (SomeSink (sink :: Sink sinkT)) = do
  (m_source :: Source m, m_sink :: Sink m) <- newTypedPipe
  env <- ask
  left <- liftIO . async $ runReaderT (runExceptT (catchIO $ spawnPipeline jm job (SomePipeline p1) (SomeSource source) (SomeSink m_sink))) env
  right <- liftIO . async $ runReaderT (runExceptT (catchIO $ spawnPipeline jm job (SomePipeline p2) (SomeSource m_source) (SomeSink sink))) env
  
  first <- liftIO $ waitEitherCatch left right
  case first of
    Left (Left e) -> throwError $ Unknown $ displayException e
    Left (Right (Left e)) -> throwError e
    Left (Right (Right _)) -> do
      liftIO (waitCatch right) >>= \case
        Left e -> throwError $ Unknown $ displayException e
        Right (Left e) -> throwError e
        Right (Right ec) -> pure ec
    Right (Left e) -> liftIO (cancel left) >> throwError (Unknown $ displayException e)
    Right (Right (Left e)) -> liftIO (cancel left) >> throwError e
    Right (Right (Right ec)) -> liftIO (cancel left) >> pure ec

spawnPipeline jm job (SomePipeline (Followed (p1 :: Pipeline i o) (p2 :: Pipeline i o) condition)) (SomeSource (source :: Source sourceT)) (SomeSink (sink :: Sink sinkT)) = do
  left <- spawnPipeline jm job (SomePipeline p1) (SomeSource source) (SomeSink sink)
  condition left >>= bool
      (pure left)
      (spawnPipeline jm job (SomePipeline p2) (SomeSource source) (SomeSink sink))

shellNodeAdapter :: (Typeable i, Typeable a, PrettyAnn ShellAnn a) => Pipeline i a -> Pipeline i ShellNode
shellNodeAdapter = Adapter $ pure . ShellNode
