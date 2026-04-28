module FokShell.Module.Preprocessor where
import Language.Parser (Node)

type Preprocessor = Node -> IO Node

connectPreprocessors :: [Preprocessor] -> Preprocessor
connectPreprocessors [] n = pure n
connectPreprocessors (x:xs) n = x n >>= connectPreprocessors xs
