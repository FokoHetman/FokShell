{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.DirectoryTracker where
import FokShell.Module
import FokShell.Types
import Data.Bool (bool)
import FokShell.Utils
import Data.Data (Proxy(Proxy))
import System.Directory (getCurrentDirectory)

import Data.Text qualified as T
import FokShell.Module.Parser
--import FokShell.Module.Preprocessor.StringPreprocessors (substituteprefix)
--import FokShell.Module.Preprocessor (Preprocessor)
import Lib.Primitive
import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))

data DirectoryTrackerConfig = DirectoryTrackerConfig {}
instance Def DirectoryTrackerConfig where
  def = DirectoryTrackerConfig
data DirectoryTracker = DirectoryTracker
  {
    directories :: TVar [FilePath]
  }

instance Configurable DirectoryTracker DirectoryTrackerConfig where
  configure _ = do
    directories <- liftIO $ newTVarIO []
    pure DirectoryTracker { directories }

--directoryPreprocessor :: Preprocessor
--directoryPreprocessor = substituteprefix "-" (\p -> T.pack <$> getPrev p)

--getPrev :: ShellConfig -> IO FilePath
--getPrev p = case requestModule @DirectoryTracker p.modules of
--  (x:_) -> do
--    dirs <- readTVarIO x.directories
--    case head' =<< tail' dirs of
--      Just x' -> pure x'
--      Nothing -> getCurrentDirectory
--  _ -> getCurrentDirectory

instance Module' DirectoryTracker ShellAction where
  initHook tc = do
      --let new_p = bool p (modifyModule' (Proxy @ParserModule) p (\mgr -> mgr {preprocessors = directoryPreprocessor:mgr.preprocessors}))
      {-when tc.addPreprocessor $ do
        mapM_ (atomically . (`modifyTVar` \pm -> pm {preprocessors = directoryPreprocessor:pm.preprocessors})) $ requestModule @ParserModule conf'.modules-}
      currentDir <- liftIO getCurrentDirectory
      liftIO . atomically . modifyTVar tc.directories $ (currentDir:)
  exitHook tc = pure ()
  resetHook tc = pure True
  preHook tc _ = pure True
  postHook tc _ = pure True
