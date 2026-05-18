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

getPrev :: ShellProcess -> IO FilePath
getPrev p = case requestModule (Proxy @DirectoryTracker) p.shellConfig.modules of
  (x:_) -> case head' =<< tail' x.directories of
    Just x' -> pure x'
    Nothing -> getCurrentDirectory
  _ -> getCurrentDirectory

instance Module' DirectoryTracker ShellProcess where
  initHook' tc p = do
      let new_p = bool p (modifyModule' (Proxy @ParserModule) p (\mgr -> mgr {preprocessors = directoryPreprocessor:mgr.preprocessors})) tc.addPreprocessor
      currentDir <- getCurrentDirectory
      pure (tc {directories = currentDir:tc.directories},new_p)
  exitHook' tc p = pure (tc,p)
  resetHook' tc p = pure (tc,p)
  preHook' tc p _ = pure (True, (tc,p))
  postHook' tc p _ = pure (True, (tc,p))
