module FokShell.Module.JobManager where

import Data.Text qualified as T
import FokShell.JobManager
import Lib.Config
import FokShell.Module
import Lib.Keys
import FokShell.Types
data JobManagerModule = JobManagerModule
  {
    jobs :: [Job]
  }

instance Module' JobManagerModule ShellProcess where
  initHook' tc p = pure (tc,p)
  preHook' tc p e = case e of
    (KeyModifiers 0, Enter) -> do
          swallowPrompt (cursorLoc conf) (input conf) (prompt conf $ colorScheme conf)
          putStrLn ""
          (job, p') <- handleJob p {shellConfig = conf {history = T.strip (input conf):history conf, historyIndex = Nothing}}
          displayPrompt (prompt p'.shellConfig $ colorScheme p'.shellConfig)
          case job of 
            Just x -> pure (False, (tc {jobs = x:jobs tc}, p'))
            Nothing -> pure (False, (tc, p'))
    _ -> pure (True, (tc, p))
    where
    conf = p.shellConfig
  postHook' tc p = pure (tc,p)
