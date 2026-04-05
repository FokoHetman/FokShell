module Language.Parser where

import Data.Map qualified as Map
import Data.Text qualified as T
import Control.Applicative
import Data.Char (isSpace)
import Data.Tuple (swap)
import Data.Bool (bool)

data StdMode = Stdout | Stderr deriving (Eq, Ord, Show)
data PipeType = ProcessPipe | Write StdMode | Append StdMode | Read deriving (Eq, Ord, Show)

data Node = {-operations-}  ProcessCall Node [Node] | And Node Node | Pipe PipeType Node Node | Sequence Node Node | Or Node Node |
            {-abstract-}    Table (Map.Map Node Node) | Array [Node] |
            {-primitives-}  Path FilePath | NodeString T.Text
    deriving (Eq, Ord, Show)
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

instance Monad Parser where
  Parser pa >>= f = Parser $ \input -> do
    (rest, a) <- pa input
    runParser (f a) rest


pipe :: Parser Node
pipe = f (Append Stdout) (stringP ">>")
      <|> f (Write Stderr) (stringP ">2")
      <|> f (Append Stderr) (stringP ">>2")
      <|> f (Write Stdout) (charP '>')
      <|> f Read (charP '<')
      <|> f ProcessPipe (stringP "|")
  where
    f sx ssx = Pipe sx <$> (parseExpr'' <* ssx) <*> parseExpr''

andand :: Parser Node
andand = And <$> (parseExpr' <* stringP "&&") <*> parseExpr

sequenceP :: Parser Node
sequenceP = Sequence <$> (parseExpr <* stringP ";") <*> parseSeq

parseSeq, parseExpr, parseExpr', parseExpr'' :: Parser Node
parseSeq = sequenceP <|> parseExpr
parseExpr = andand <|> parseExpr'
parseExpr' = pipe <|> parseExpr''
parseExpr'' = jsontable <|> pcall


jsontable :: Parser Node
jsontable = Table <$> (ws *> charP '{' *> statements <* charP '}' <* ws)
  where
    statements = Map.fromList <$> sepBy (charP ',') (statement ':')

{-
table :: Parser Node
table = Table <$> (ws *> charP '{' *> statements <* charP '}' <* ws)
  where
    statements = Map.fromList <$> sepBy (charP ';') (statement '=')
-}
statement :: Char -> Parser (Node, Node)
statement sep = (,) <$> (ws *> shellWord <* ws <* charP sep) <*> (ws *> parseExpr <* ws)


pcall :: Parser Node
pcall = ProcessCall <$> (ws *> shellWord <* ws) <*> (many (ws *> shellWord <* ws))

ws :: Parser T.Text
ws = spanP isSpace

wsForce = do
  s <- spanP isSpace
  if T.null s then empty else pure s

item :: Parser Char
item = Parser $ \t -> swap <$> T.uncons t

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  if p c then pure c else empty

charP :: Char -> Parser Char
charP c = satisfy (==c)

specialChars :: String
specialChars = "=:;{}<|>,!#&/\\\"' "

isSpecial :: Char -> Bool
isSpecial = (`elem` specialChars)

bare, singleQuoted, doubleQuoted, shellWordP :: Parser T.Text
bare = spanPForce (not . isSpecial)
singleQuoted = charP '\'' *> spanP (/='\'') <* charP '\''
doubleQuoted = charP '"' *> spanP (/='\"') <* charP '"'
shellWordP = singleQuoted <|> doubleQuoted <|> bare

shellWord :: Parser Node
shellWord = NodeString <$> shellWordP

stringP :: String -> Parser String
stringP = traverse charP

spanP, spanPForce :: (Char -> Bool) -> Parser T.Text
spanP f = Parser $ \input ->
  let (token, rest) = T.span f input
  in Just (rest, token)

spanPForce f = do
  res <- spanP f
  bool (pure res) empty (T.null res)

sepBy :: Parser a
      -> Parser b
      -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

