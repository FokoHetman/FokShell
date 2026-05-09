{-# LANGUAGE LambdaCase, GADTs #-}
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
import Control.Monad (filterM, join)
import Debug.Trace (traceShow)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Data (Typeable, Proxy, cast)
import GHC.IO.Exception (ExitCode)
import System.IO (IOMode)
import GHC.IO.Handle
import Control.Concurrent (MVar, readMVar, putMVar, isEmptyMVar)
import Data.Maybe (isJust, fromJust)


data TaskPipeType = File FilePath IOMode | Terminal | ProcessData (MVar (Either Node Handle))

data Task = Task {
  procName    :: T.Text
, procArgs    :: [T.Text]
, pipeIn      :: TaskPipeType
, pipeOut     :: TaskPipeType
, pipeErr     :: TaskPipeType
, prevTask    :: Maybe Task
, condition   :: ExitCode -> Bool
}

class Node' a where
  parse       :: Parser Node -> Parser a
  nodeLen' :: a -> Int
  nodeToText' :: a -> T.Text
  modifyNode' :: a -> (Node -> Node) -> Node
  makeTask'   :: a -> IO Task
  getRawData' :: a -> Int -> ({- most primitive node the cursor is on -} Node, {- all strings before the cursor, including current one -}[T.Text], {- index into the current word -} Int)

data Node where
  Node :: (Node' a,Typeable a) => a -> Node

makeTask :: Node -> IO Task
makeTask (Node a) = makeTask' a

nodeLen :: Node -> Int
nodeLen (Node a) = nodeLen' a
nodeToText :: Node -> T.Text
nodeToText (Node a) = nodeToText' a

modifyNode :: Node -> (Node -> Node) -> Node
modifyNode (Node a) = modifyNode' a

getRawData :: Node -> Int -> (Node, [T.Text], Int)
getRawData (Node a) = getRawData' a


withProxyNode :: forall i. Typeable i => Proxy i -> Node -> Maybe i
withProxyNode _ (Node a) = cast a
requestNode :: forall a. (Node' a,Typeable a) => Proxy a -> [Node] -> [a]
requestNode p xs = fmap fromJust $ filter isJust $ fmap (withProxyNode p) xs


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


-- Chain {{{
data ChainType = Sequence | OnSuccess | OnFailure
chainLen :: ChainType -> Int
chainLen Sequence = 1
chainLen OnSuccess = 2
chainLen OnFailure = 2
data ChainExp = ChainExp ChainType Node Node

instance Node' ChainExp where
  parse = chainParser
  nodeLen' (ChainExp t left right) = nodeLen left + nodeLen right + chainLen t
  nodeToText' = undefined
  modifyNode' (ChainExp t left right) f = f (Node $ ChainExp t (modifyNode left f) $ modifyNode right f)

chainParser :: Parser Node -> Parser ChainExp
chainParser lower = f Sequence (stringP ";") <|> f OnSuccess (stringP "&&") <|> f OnFailure (stringP "||")
  where
    f sx ssx = ChainExp sx <$> (lower <* ssx) <*> (Node <$> pipelineParser lower)
-- }}}

-- Pipeline {{{
data StdMode = Stdout | Stderr deriving (Eq, Ord, Show)
data Pipeline = Pipe | Write StdMode | Append StdMode | Read deriving (Eq, Ord, Show)
pipelineLen :: Pipeline -> Int
pipelineLen x
      | elem x [Pipe, Write Stdout, Read] = 1
      | elem x [Write Stderr, Append Stdout] = 2
      | elem x [Append Stderr] = 3
      | otherwise = traceShow "0: shouldn't happen" 0

data PipelineExp = PipelineExp Pipeline Node Node

instance Node' PipelineExp where
  parse = pipelineParser
  nodeLen' (PipelineExp pline left right) = nodeLen left + nodeLen right + pipelineLen pline
  nodeToText' = undefined
  modifyNode' (PipelineExp pline left right) f = f (Node $ PipelineExp pline (modifyNode left f) $ modifyNode right f)

pipelineParser :: Parser Node -> Parser PipelineExp
pipelineParser lower = f (Append Stdout) (stringP ">>")
      <|> f (Write Stderr) (stringP ">2")
      <|> f (Append Stderr) (stringP ">>2")
      <|> f (Write Stdout) (stringP ">")
      <|> f Read (stringP "<")
      <|> f Pipe (stringP "|")
  where
    f sx ssx = PipelineExp sx <$> (lower <* ssx) <*> (Node <$> pipelineParser lower)
-- }}}

-- ProcessCall {{{
data ProcessCall = ProcessCall Node [Node]

instance Node' ProcessCall where
  parse  = pcallParser
  nodeLen' (ProcessCall n ns) = nodeLen n + sum (fmap nodeLen ns)
  nodeToText' _ = undefined
  modifyNode' (ProcessCall n ns) f = f (Node $ ProcessCall (modifyNode n f) $ fmap (`modifyNode` f) ns)

pcallParser :: Parser Node -> Parser ProcessCall
pcallParser lower = ProcessCall <$> (ws *> lower <* ws) <*> (many (ws *> lower <* ws))
-- }}}

-- Primitive {{{
data QuoteType = None | SingleQuote | DoubleQuote
data Primitive = NodeString T.Text QuoteType

instance Node' Primitive where
  parse _ = nodestringParser
  nodeLen' (NodeString t None) = T.length t
  nodeLen' (NodeString t _) = T.length t + 2
  nodeToText' (NodeString t _) = t
  modifyNode' n f = f (Node n)

nodestringParser :: Parser Primitive
nodestringParser = uncurry NodeString <$> nodestringP

bare, singleQuoted, doubleQuoted, nodestringP :: Parser (T.Text,QuoteType)
bare = (,None) <$> spanPForce (not . isSpecial)
singleQuoted = (,SingleQuote) <$> (charP '\'' *> spanP (/='\'') <* charP '\'')
doubleQuoted = (,DoubleQuote) <$> (charP '"' *> spanP (/='\"') <* charP '"')
nodestringP = singleQuoted <|> doubleQuoted <|> bare

-- }}}

ws, wsForce :: Parser T.Text
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


{-
nodeToString :: Node -> T.Text
nodeToString (NodeString s _) = s
nodeToString (ProcessCall x xs) = nodeToString x <> T.concat (fmap nodeToString xs)
nodeToString x = traceShow x undefined

nlength :: Node -> Int
nlength (NodeString s _) = T.length s
nlength (Path p) = length p
nlength (Set t) = {- {} -} 2 + sum (fmap (uncurry (+) . join bimap nlength) $ Map.toList t)
nlength (Array a) = sum $ fmap nlength a
nlength (ProcessCall p as) = nlength p + sum (fmap nlength as)
nlength (And n1 n2) = nlength n1 + nlength n2 + 2 {- && -}
nlength (Or n1 n2) = nlength n1 + nlength n2 + 2 {- || -}
nlength (Pipe ps n1 n2) = nlength n1 + nlength n2 + pipelength ps
nlength (Sequence n1 n2) = nlength n1 + nlength n2 + 1 {- ; -}

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
parseExpr'' = jsonset <|> jsonarray <|> pcall

csvtable :: Char -> Parser Node
csvtable separator = Table <$> (ws *> body <* ws)
  where
    body = sepBy (charP '\n') $ row separator

row :: Char -> Parser [Node]
row separator = sepBy (charP separator) parseExpr


jsonarray :: Parser Node
jsonarray = Array <$> (ws *> charP '{' *> body <* charP '}' <* ws)
  where
    body = sepBy (charP ',') parseExpr


jsonset :: Parser Node
jsonset = Set <$> (ws *> charP '{' *> statements <* charP '}' <* ws)
  where
    statements = Map.fromList <$> sepBy (charP ',') (statement ':')

statement :: Char -> Parser (Node, Node)
statement sep = (,) <$> (ws *> shellWord <* ws <* charP sep) <*> (ws *> parseExpr <* ws)


pcall :: Parser Node
pcall = ProcessCall <$> (ws *> shellWord <* ws) <*> (many (ws *> shellWord <* ws))

ws, wsForce :: Parser T.Text
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

bare, singleQuoted, doubleQuoted, shellWordP :: Parser (T.Text,Bool)
bare = (,True) <$> spanPForce (not . isSpecial)
singleQuoted = (,False) <$> (charP '\'' *> spanP (/='\'') <* charP '\'')
doubleQuoted = (,True) <$> (charP '"' *> spanP (/='\"') <* charP '"')
shellWordP = singleQuoted <|> doubleQuoted <|> bare

shellWord :: Parser Node
shellWord = uncurry NodeString <$> shellWordP

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
isValidArgument rules (execuset:args') = case lookupRule execuset rules of
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

-}

-- vim: foldmethod=marker
