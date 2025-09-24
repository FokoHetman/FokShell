module JobManager where

import qualified Data.Text as T
import ExposedTypes
import Control.Applicative

handleJob :: ShellProcess -> IO ShellProcess
handleJob (ShellProcess conf state) = do
  putStrLn $ "Job Manager handling: " ++ T.unpack (input conf)
  
  let _ = argumentize $ tree $ input conf


  pure (ShellProcess conf state)

newtype Tree = Tree [Node]
data Node = String T.Text | Call [Node] | Variant [Node]
  deriving (Show,Eq)

newtype Parser a = Parser {runParser :: T.Text -> Maybe (T.Text, a)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input',x) <- p input
      Just (input',f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a)<- p2 input'
    Just (input'', f a)


instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input


tree :: T.Text -> Tree
tree t = undefined --construct a tree from parseExpr

parseExpr = call <|> variant <|> string

string :: Parser Node
string = String <$> chars <* charP ' '

charP :: Char -> Parser Char
charP x = Parser f'
  where
    f' t = f $ T.unpack t
    f (y:ys)
      | y==x = Just (T.pack ys, x)
      | otherwise = Nothing
    f [] = Nothing

chars :: Parser T.Text
chars = spanP (/=' ')

spanP :: (Char -> Bool) -> Parser T.Text
spanP f = Parser $ \input ->
  let (token, rest) = span f $ T.unpack input
  in Just (T.pack rest, T.pack token)



argumentize :: Tree -> [T.Text]
argumentize = undefined
