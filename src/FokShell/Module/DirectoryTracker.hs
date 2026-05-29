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
import FokShell.Module.Preprocessor.StringPreprocessors (substituteprefix)
import FokShell.Module.Preprocessor (Preprocessor)
import Lib.Primitive
import Control.Concurrent.STM
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (when)

data DirectoryTracker = DirectoryTracker
  {
    directories :: [FilePath]
  , addPreprocessor :: Bool
  }

instance Def DirectoryTracker where
  def = DirectoryTracker
    { directories = []
    , addPreprocessor = True
    }

directoryPreprocessor :: Preprocessor
directoryPreprocessor = substituteprefix "-" (\p -> T.pack <$> getPrev p)

getPrev :: ShellConfig -> IO FilePath
getPrev p = case requestModule @DirectoryTracker p.modules of
  (x':_) -> do
    x <- readTVarIO x'
    case head' =<< tail' x.directories of
      Just x' -> pure x'
      Nothing -> getCurrentDirectory
  _ -> getCurrentDirectory

instance Module' DirectoryTracker ShellConfig where
  initHook' tc conf = do
      conf' <- readTVarIO conf
      tc'   <- readTVarIO tc
      --let new_p = bool p (modifyModule' (Proxy @ParserModule) p (\mgr -> mgr {preprocessors = directoryPreprocessor:mgr.preprocessors}))
      when tc'.addPreprocessor $ do
        mapM_ (atomically . (`modifyTVar` \pm -> pm {preprocessors = directoryPreprocessor:pm.preprocessors})) (requestModule @ParserModule conf'.modules)
      currentDir <- getCurrentDirectory
      atomically . modifyTVar tc $ \tc' -> tc' {directories=currentDir:tc'.directories}
  exitHook' tc p = pure ()
  resetHook' tc p = pure True
  preHook' tc p _ = pure True
  postHook' tc p _ = pure True
