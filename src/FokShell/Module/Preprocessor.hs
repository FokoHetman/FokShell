module FokShell.Module.Preprocessor where
import Language.Parser (Node)
import FokShell.Types (ShellConfig)

type Preprocessor = ShellConfig -> Node -> IO Node

connectPreprocessors :: [Preprocessor] -> Preprocessor
connectPreprocessors [] _ n = pure n
connectPreprocessors (x:xs) p n = x p n >>= connectPreprocessors xs p
