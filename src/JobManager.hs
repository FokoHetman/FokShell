{-# LANGUAGE OverloadedStrings,LambdaCase #-}
module JobManager where

import qualified Data.Text as T
import ExposedTypes
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Debug.Trace (trace, traceShow)
import Control.Arrow (Arrow(second))

-- implementing Call:
-- create a function: call :: T.Text (prog) -> [T.Text] (args) -> PID 
-- job manage that.



handleJob :: ShellProcess -> IO ShellProcess
handleJob (ShellProcess conf state) = do
  putStrLn $ "Job Manager handling: " ++ T.unpack (input conf)
  
  --print $ runParser parseExpr $ input conf

  --let string = stringify unodes
  --print string
  
  --evaluateNodes (ShellProcess conf state) unodes
  pure (ShellProcess conf state)


data StringComplex = Basic T.Text | Variant [StringComplex] | Combination [StringComplex]
  deriving (Show,Eq)
type Executable = StringComplex
type Args       = [StringComplex]

data Node = ProgramCall Executable Args | And Node Node
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


instance Monad Parser where
  (>>=) as bs = do 
    a <- as
    bs a


parseExpr :: Parser Node
parseExpr = pcall <|> andand


-- parser for classic && operator

-- blockers are chars that symbolize a beggining of a new NODE, such as AND (`&&`). docs/parsing:1.2 (todo)
blockers :: String
blockers = " $&><|"

-- blockers that also block stringcomplex parsing
stringblockers :: String
stringblockers = blockers ++ " {,}()"


andand :: Parser Node
andand = And <$> (parseExpr <* ws <* stringP "&&" <* ws) <*> parseExpr

ws :: Parser T.Text
ws = spanP isSpace

sepBy :: Parser a
      -> Parser b
      -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

-- parser for program call, docs/parsing:1.1
pcall :: Parser Node
{-pcall = Parser f 
  where
    f :: T.Text -> Maybe (T.Text, Node)
    f i = traceShow i $ do
      (i', mprogram) <- second capStr <$> runParser (ws *> extractUntil " &" <* charP ' ' <* ws) i
      (i'', margs) <- runParser args i'
      program <- mprogram
      _args <- sequence margs
      Just (i'', ProgramCall program _args)

args :: Parser [Maybe StringComplex] = many (capStr <$> (ws *> extractUntil " &" <* charP ' ' <* ws))
-}
pcall = ProgramCall . capStr
        <$> (ws *> extractUntil blockers <* charP ' ' <* ws)
        <*> many (capStr <$> (ws *> extractUntil blockers <* charP ' ' <* ws))


capStr :: T.Text -> StringComplex
capStr t = f (runParser stringify t)
  where 
    f t = case t of
      Just ("", x) -> x
      Just (r,  x) -> case capStr r of
        Combination y -> Combination (x:y)
        y             -> Combination [x, y]
      Nothing -> error "capStr died lmao"
stringify :: Parser StringComplex
stringify = basic <|> variant



basic :: Parser StringComplex
basic = Basic <$> extractUntil stringblockers

variant :: Parser StringComplex
variant = Variant <$> (charP '{' *> ws *> variants <* ws <* charP '}')
  where
    variants = sepBy (ws *> charP ',' <* ws) stringify

extractUntil :: [Char] -> Parser T.Text
extractUntil b = Parser f'
  where
    f' t = result
      where
        parsed = f t
        rest = T.takeEnd (T.length t - T.length parsed) t
        result =  if T.null parsed then Nothing else Just (rest, parsed)
    f t
      | T.null t          = T.empty
      | T.head t `elem` b = T.empty
      | otherwise         = T.concat [T.singleton $ T.head t, f (T.drop 1 t)]


{-

parseExpr :: Parser Node
parseExpr = trace "called" $ eol <|> strings <|> envVars <|> variants

openers = ['{', '(', '"', '$']
openers, limited, blockers :: [Char]
limited = openers ++ "$,})\""
blockers = ' ':limited

strings :: Parser Node
strings = strings' <|> trace "strings" (String <$> (ws *> trace "extract" extract blockers) <*> parseExpr)

strings' :: Parser Node
strings' = trace "strings'" $ String <$> (charP '"' *> trace "extract" extract limited <* charP '"') <*> parseExpr

eol :: Parser Node
eol = Parser $ \x -> if x == T.empty || (T.head x `elem` blockers && T.head x `notElem` openers) then Just (x, EOL) else Nothing
--separator :: Parser Node
--separator = Parser $ \x -> if x /= T.empty && T.head x == ' ' then Just (T.drop 1 x, Space) else Nothing

envVars :: Parser Node
envVars = EnvVar <$> (charP '$' *> extract blockers) <*> parseExpr


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


-}

charP :: Char -> Parser Char
charP x = Parser f
  where
    f t
      | t == T.empty = Just("", x)
      | T.head t==x = Just (T.drop 1 t, x)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP



spanP :: (Char -> Bool) -> Parser T.Text
spanP f = Parser $ \input ->
  let (token, rest) = T.span f input
  in Just (rest, token)
