{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Parser where
import Language.Parser
import Lib.Primitive

import Control.Applicative
import Data.Text qualified as T

import FokShell.Types (ShellProcess)
import FokShell.Module
import FokShell.Module.Preprocessor
import FokShell.Module.Preprocessor.StringPreprocessors
import System.Directory (getHomeDirectory)

data ParserModule = ParserModule
  { parser :: Parser Node
  , preprocessors :: [Preprocessor]
  }

instance Def ParserModule where
  def = ParserModule
    { parser = r3
    , preprocessors = [connectPreprocessors [substituteprefix "~" (const $ T.pack <$> getHomeDirectory), envVarPreprocessor]]
    }
r0 = primitives empty
r1 = pcall r0 <|> r0
r2 = pipes r1 <|> r1
r3 = chains r2 <|> r2

instance Module' ParserModule ShellProcess where
  initHook' tc p = pure (tc,p)
  exitHook' tc p = pure (tc,p)
  resetHook' tc p = pure (tc,p)
  preHook' tc p _ = pure (True,(tc,p))
  postHook' tc p _ = pure (True,(tc,p))

primitives, pcall, pipes, chains :: Parser Node -> Parser Node
primitives lower = Node <$> (parse lower :: Parser Primitive)
pcall lower = Node <$> (parse lower :: Parser ProcessCall)
pipes lower = Node <$> (parse lower :: Parser PipelineExp)
chains lower = Node <$> (parse lower :: Parser ChainExp)
