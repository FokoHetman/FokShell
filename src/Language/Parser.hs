{-# LANGUAGE LambdaCase #-}
module Language.Parser where

import Data.Map qualified as Map
import Data.Text qualified as T
import Control.Applicative
import Data.Char (isSpace)
import Data.Tuple (swap)
import Data.Bool (bool)
import Data.Functor
import System.Directory
import System.FilePath
import Control.Monad (filterM)
import Debug.Trace (traceShow)

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


nodeToString :: Node -> T.Text
nodeToString (NodeString s) = s
nodeToString (ProcessCall x xs) = nodeToString x <> T.concat (fmap nodeToString xs)
nodeToString x = traceShow x undefined

nlength :: Node -> Int
nlength = undefined

pipelength :: PipeType -> Int
pipelength t
            | t `elem` [ProcessPipe, Write Stdout, Write Stderr, Read] = 1
            | t `elem` [Append Stdout, Append Stderr] = 2
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
specialChars = "=:;{}<|>,!#&\\\"' "

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



data CompletionRule = CompRule T.Text (T.Text -> IO [CompletionRule])

instance Show CompletionRule where
  show (CompRule x _) = "CompRule `" ++ T.unpack x ++ "`"


unwrapArgs :: CompletionRule -> [T.Text] -> IO [CompletionRule]
unwrapArgs (CompRule _ f) [t] = f t
unwrapArgs (CompRule _ f) (t:ts) = f t >>= \case
    [CompRule x f2] -> if x==t then unwrapArgs (CompRule x f2) ts else pure []
    _ -> pure []
unwrapArgs _ [] = pure []
-- todo: add completions and file cache to this
isValidArgument :: [CompletionRule] -> [T.Text] -> IO Bool
isValidArgument rules (executable:args') = case lookupRule executable rules of
  Just (CompRule x f) -> unwrapArgs (CompRule x f) args' <&> \case
    [CompRule x2 _] -> x2==last args'
    _   -> False
  Nothing             -> pure True
isValidArgument _ [] = pure True

lookupRule :: T.Text -> [CompletionRule] -> Maybe CompletionRule
lookupRule t (CompRule x f:xs) = bool (lookupRule t xs) (Just $ CompRule x f) (t==x)
lookupRule _ [] = Nothing


nestNTimes :: CompletionRule -> [T.Text] -> Int -> IO [CompletionRule]
nestNTimes (CompRule _ f) (t:_) 0 = f t
nestNTimes (CompRule _ f) (t:ts) n = f t >>= \case
  [CompRule t2 f2] -> if t==t2 then nestNTimes (CompRule t2 f2) ts (n-1) else pure []
  _ -> pure []
nestNTimes _ [] _ = pure []



fileCompletion :: (FilePath -> IO Bool) -> (T.Text -> IO [CompletionRule]) -> (T.Text -> IO [CompletionRule])
fileCompletion filtre nest t = do
    let d = takeDirectory $ T.unpack t
    exists <- doesDirectoryExist d
    if exists then getPermissions d >>= \x ->
      if readable x then do
        localFiles <- getDirectoryContents d >>= filterM (filtre . (d</>))
        let matches = filter (T.isPrefixOf t) $ bool id (T.pack . (d</>) . T.unpack) (T.pack d `T.isPrefixOf` t) <$> fmap T.pack localFiles
        pure $ fmap (`CompRule` nest) matches
      else pure []
    else pure []
fileCompletionRec :: (FilePath -> IO Bool) -> T.Text -> IO [CompletionRule]
fileCompletionRec filtr = fileCompletion filtr (fileCompletionRec filtr)

fileListCompletion :: (FilePath -> IO Bool) -> T.Text -> CompletionRule
fileListCompletion filtr = (`CompRule` fileCompletionRec filtr)
