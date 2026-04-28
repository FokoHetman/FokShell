{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.JobManager where

import Data.Text qualified as T
import Data.Functor ((<&>))
import FokShell.JobManager
import Lib.Config
import FokShell.Module
import Lib.Keys
import FokShell.Types
import Language.Parser
import FokShell.Module.Preprocessor
data JobManagerModule = JobManagerModule
  {
    jobs :: [Job]
  , preprocessors :: [Preprocessor]
  }

instance Module' JobManagerModule ShellProcess where
  initHook' tc p = pure (tc,p)
  preHook' tc p e = case e of
    (KeyModifiers 0, Enter) -> do
          swallowPrompt (cursorLoc conf) (input conf) (prompt conf $ colorScheme conf)
          putStrLn ""
          let conf = shellConfig p
          let input' = T.strip $ input conf
          --let task = mkTask' $ T.strip $ input conf
          let preprocess = connectPreprocessors tc.preprocessors
          let task = runParser parseSeq input' <&> (>>= mkTask) . preprocess . snd
          (job, p') <- case task of
            Just t  -> t >>= \t -> do
              let job = Job t
              p <- spawnJob (p {shellConfig = conf { input="", cursorLoc=0 }}) job
              pure (Just job, p)
            Nothing -> pure (Nothing, p {shellConfig = conf {input="",cursorLoc=0}})
          --(job, p') <- handleJob p {shellConfig = conf {history = T.strip (input conf):history conf, historyIndex = Nothing}}
          displayPrompt (prompt p'.shellConfig $ colorScheme p'.shellConfig)
          case job of 
            Just x -> pure (False, (tc {jobs = x:jobs tc}, p'))
            Nothing -> pure (False, (tc, p'))
    _ -> pure (True, (tc, p))
    where
    conf = p.shellConfig
  postHook' tc p = pure (tc,p)
