{-# LANGUAGE OverloadedStrings,LambdaCase #-}
module JobManager where

import qualified Data.Text as T
import ExposedTypes
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Debug.Trace (trace)

handleJob :: ShellProcess -> IO ShellProcess
handleJob (ShellProcess conf state) = do
  putStrLn $ "Job Manager handling: " ++ T.unpack (input conf)
  
  --print $ runParser parseExpr $ input conf
  let nodes = getNodes $ input conf
  print nodes

  let unodes = cognifyNodes nodes
  print unodes

  

  --let args = tree $ input conf
  --print args

  pure (ShellProcess conf state)

evaluateNodes :: ShellProcess -> [Node] -> IO ()
evaluateNodes (ShellProcess conf state) ((String text node):nodes) = undefined--do
    --asText node -- rethink this.


cognifyNodes :: Maybe [Node] -> Maybe [Node]
cognifyNodes nodes = map walkNode <$> nodes

walkNode :: Node -> Node
walkNode = \case
  String t n -> case walkNode n of 
    String t2 n2 -> walkNode (String (T.concat [t,t2]) n2)
    Variant v n2 -> walkNode (Variant (fmap (walkNode . String t) v) n2)
    EOL          -> String t n
  Variant v n -> case walkNode n of
    String t n2 -> walkNode (Variant (fmap (walkNode . (injectEnd (String t EOL))) v) n2)
    Variant v2 n2 -> walkNode (Variant [walkNode (injectEnd (Variant v2 EOL) x)|x<-v] n2)
    EOL -> Variant v n
  x -> x 

injectEnd :: Node -> Node -> Node
injectEnd i (String t n) = String t $ injectEnd i n
injectEnd i (Variant v n) = Variant v $ injectEnd i n
injectEnd i EOL = i

getNodes :: T.Text -> Maybe [Node]
getNodes t = if t == T.empty then Just [] else do
  (r, n :: Node) <- runParser parseExpr $ T.strip t
  
  trace ("rest: " ++ T.unpack r ++ " and n: " ++ show n) getNodes r >>= f n
  where
    f x y = Just (x:y)

data Node = String T.Text Node | Call Node | Variant [Node] Node | Environment T.Text | Space | EOL
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


tree :: T.Text -> Node
tree = undefined

parseExpr :: Parser Node
parseExpr = trace "called" $ eol <|> strings <|> variants


--strings :: Parser Node
--strings = String <$> (charP ' ' *> spanP (/=' ') <* charP ' ') <*> parseExpr

openers = ['{', '(']
blockers :: [Char]
blockers = openers ++ "$, })"

strings :: Parser Node
strings = trace "strings" $ String <$> (ws *> trace "extract" extract blockers) <*> parseExpr

eol :: Parser Node
eol = Parser $ \x -> if x == T.empty || (T.head x `elem` blockers && T.head x `notElem` openers) then Just (x, EOL) else Nothing
--separator :: Parser Node
--separator = Parser $ \x -> if x /= T.empty && T.head x == ' ' then Just (T.drop 1 x, Space) else Nothing

variants :: Parser Node
variants = Variant <$> (charP '{' *> ws *> variants' <* ws <* charP '}') <*> trace "parseExpr from variants" parseExpr


variants' :: Parser [Node]
variants' = sepBy (ws *> trace "wooohoo" charP ',' <* ws) parseExpr

sepBy :: Parser a
      -> Parser b
      -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

ws :: Parser T.Text
ws = spanP isSpace

extract :: [Char] -> Parser T.Text
extract b = Parser f'
  where
    f' t = trace ("extract: " ++ T.unpack t) result
      where
        parsed = f t
        rest = T.takeEnd (T.length t - T.length parsed) t
        result = if T.null parsed then Nothing else
            Just (rest, parsed)
    f t
      | t == T.empty      = T.empty
      | T.head t `elem` b = T.empty
      | otherwise         = T.concat [T.singleton $ T.head t, f (T.drop 1 t)]
extractChar :: Char -> [Char] -> Maybe Char
extractChar c blockers
  | c `elem` blockers = Nothing
  | otherwise = Just c

charP :: Char -> Parser Char
charP x = Parser f
  where
    f t
      | t == T.empty = Just("", x)
      | T.head t==x = Just (trace ("IMPRO: " ++ [x] ++ " :: " ++ T.unpack t) $ T.drop 1 t, x)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP



spanP :: (Char -> Bool) -> Parser T.Text
spanP f = Parser $ \input ->
  let (token, rest) = T.span f input
  in Just (rest, token)

argumentize :: Node -> [T.Text]
argumentize = undefined
