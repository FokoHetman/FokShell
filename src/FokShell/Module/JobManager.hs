{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.JobManager where

import Data.Text qualified as T
import Data.Functor ((<&>))
import FokShell.JobManager
import FokShell.Types
import FokShell.Module
import FokShell.Utils
import Lib.Keys
import Language.Parser
import FokShell.Module.Preprocessor
import Control.Concurrent (newEmptyMVar)
import FokShell.Module.Prompt (displayPrompt')
import Lib.Primitive
import FokShell.Module.Preprocessor.StringPreprocessors
import System.Directory (getHomeDirectory)
import FokShell.Module.Preprocessor (connectPreprocessors)
import FokShell.Module.Parser
import Data.Data (Proxy(Proxy))
data JobManager = JobManager
  {
    jobs :: [Job]
  }

instance Def JobManager where
  def = JobManager
    { jobs = []
    }

instance Module' JobManager ShellProcess where
  initHook' tc p = pure (tc,p)
  exitHook' tc p = pure (tc, p)
  resetHook' tc p = pure (tc, p)
  preHook' tc p e = case e of
    (KeyModifiers 0, Enter) -> do
          putStrLn ""
          let conf = shellConfig p
          let input' = T.strip $ input conf
          let parser = case requestModule (Proxy @ParserModule) conf.modules of
                    (x:_) -> x
                    _ -> def
          let preprocess = connectPreprocessors parser.preprocessors
          let task = runParser parser.parser input' <&> (>>= makeTask) . preprocess p . snd
          (job, p') <- case task of
            Just t' -> t' >>= \t -> do
              mvar <- newEmptyMVar
              let job = Job t mvar
              p' <- spawnJob (p {shellConfig = conf { input="", cursorLoc=0 }}) job
              pure (Just job, p')
            Nothing -> pure (Nothing, p {shellConfig = conf {input="",cursorLoc=0}})

          displayPrompt' p'
          case job of 
            Just x -> pure (False, (tc {jobs = x:jobs tc}, p'))
            Nothing -> pure (False, (tc, p'))
    _ -> pure (True, (tc, p))
  postHook' tc p e = pure (True,(tc,p))
