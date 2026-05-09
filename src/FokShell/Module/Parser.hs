module FokShell.Module.Parser where
import Language.Parser
import Lib.Primitive

import Control.Applicative
import FokShell.Types (ShellProcess)
import FokShell.Module

data ParserModule = ParserModule
  { parser :: Parser Node
  }

instance Def ParserModule where
  def = ParserModule
    { parser = chains (pipes $ primitives empty) <|> pipes (primitives empty) <|> primitives empty
    }

instance Module' ParserModule ShellProcess where
  initHook' tc p = pure (tc,p)
  exitHook' tc p = pure (tc,p)
  preHook' tc p _ = pure (True,(tc,p))
  postHook' tc p _ = pure (True,(tc,p))

primitives, pipes, chains :: Parser Node -> Parser Node
primitives lower = (Node <$> (parse lower :: Parser Primitive)) <|> (Node <$> (parse lower :: Parser ProcessCall))
pipes lower = Node <$> (parse lower :: Parser PipelineExp)
chains lower = Node <$> (parse lower :: Parser Primitive)
