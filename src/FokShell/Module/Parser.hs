{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Parser where
import Language.Parser
import Lib.Primitive

import Control.Applicative
import Data.Text qualified as T

import FokShell.Module
import FokShell.Module.Preprocessor
import FokShell.Module.Preprocessor.StringPreprocessors
import System.Directory (getHomeDirectory)
import FokShell.Types

data ParserModule = ParserModule
  { parser :: Parser Node
  --, preprocessors :: [Preprocessor]
  }

instance Def ParserModule where
  def = ParserModule
    { parser = r4
    --, preprocessors = [connectPreprocessors [substituteprefix "~" (const $ T.pack <$> getHomeDirectory), envVarPreprocessor]]
    }
r0,r1,r2,r3,r4 :: Parser Node
r0 = primitives empty
r1 = pcall r0 <|> r0
r2 = pipes r1 <|> r1
r3 = chains r2 <|> r2
r4 = detach r3 <|> r3

instance Module' ParserModule ShellAction where
  initHook' _ = pure ()
  exitHook' _ = pure ()
  resetHook' _ = pure True
  preHook' _ _ = pure True
  postHook' _ _ = pure True

primitives, pcall, pipes, chains, detach :: Parser Node -> Parser Node
primitives lower = Node <$> (parse lower :: Parser Primitive)
pcall lower = Node <$> (parse lower :: Parser ProcessCall)
pipes lower = Node <$> (parse lower :: Parser PipelineExp)
chains lower = Node <$> (parse lower :: Parser ChainExp)
detach lower = Node <$> (parse lower :: Parser Detach)
