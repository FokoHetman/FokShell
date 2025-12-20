{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Language.Parser where

import qualified Data.Text as T
import Data.Functor
import Control.Applicative
import GHC.IO.IOMode
import Data.Char (isSpace)
import System.Posix (getEnv)

import Lib.ColorScheme
import Data.Maybe (isJust)
import System.Directory (findExecutable)
import Debug.Trace (traceShow)
import Control.Arrow (Arrow(second))
import Data.Bool (bool)
import Data.List (singleton)

data PipeSyntax = {- > -} Write | {- >> -} Append | {- 2> -} WriteErr | {- 2>> -} AppendErr
  deriving (Show,Eq)
data Node = ProgramCall Executable Args | And Node Node | Pipe PipeSyntax Node Node
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

-- use only in situations where it's certain node *should* be text
nodeAsText :: Node -> Maybe (IO T.Text)
nodeAsText (ProgramCall e _) = Just $ complexToText e
nodeAsText _ = Nothing


taskify :: Node -> Maybe Task
taskify (And n1 n2) = (\x y -> Task (\_ -> pure True) x (Just $ Task (\e -> pure $ e==0) y Nothing Terminal Terminal Terminal) Terminal Terminal Terminal) <$> taskify n1 <*> taskify n2
taskify (Pipe ps n1 n2) = (\x y -> Task {condition = \_ -> pure True {-probably check if n2 is a file and exists-}, body = x, next = Nothing 
  , stdinT = Terminal
  , stdoutT = case ps of 
    Write     -> File y WriteMode
    Append    -> File y AppendMode
    _         -> Terminal
  , stderrT = case ps of 
    WriteErr  -> File y WriteMode
    AppendErr -> File y AppendMode
    _      -> Terminal}
  ) <$> taskify n1 <*> nodeAsText n2
taskify (ProgramCall e a) = Just $ PCall e a
taskify x = error $ "couldn't taskify: " ++ show x

parseExpr, parseExpr', parseExpr'' :: Parser Node
parseExpr = andand <|> pipe <|> parseExpr'
parseExpr' = pcall
parseExpr'' = pcall


-- parser for classic && operator

-- blockers are chars that symbolize a beggining of a new NODE, such as AND (`&&`). docs/parsing:1.2
blockers :: String
blockers = " &2><|"

-- blockers that also block stringcomplex parsing
stringblockers :: String
stringblockers = blockers ++ " ${,}()"


pipe :: Parser Node
pipe = f Write (charP '>')
      <|> f Append (stringP ">>")
      <|> f WriteErr (stringP "2>")
      <|> f AppendErr (stringP "2>>")
  where
    f sx ssx = Pipe sx <$> (parseExpr'' <* ws <* ssx <* ws) <*> parseExpr

andand :: Parser Node
andand = And <$> (parseExpr' <* ws <* stringP "&&" <* ws) <*> parseExpr

ws :: Parser T.Text
ws = spanP isSpace

sepBy :: Parser a
      -> Parser b
      -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

-- parser for program call, docs/parsing:1.1
pcall :: Parser Node
pcall = Parser f 
  where
    f :: T.Text -> Maybe (T.Text, Node)
    f i = do
      (i', mprogram) <- second capStr <$> runParser (ws *> extractUntil " &" <* charP ' ' <* ws) i
      (i'', margs) <- runParser args i'
      program <- mprogram
      _args <- sequence margs
      Just (i'', ProgramCall program _args)

args :: Parser [Maybe StringComplex] = many (capStr <$> (ws *> extractUntil " &" <* charP ' ' <* ws))


smallLift :: Maybe (a -> b) -> Maybe a -> Maybe b
smallLift (Just f) (Just x) = Just $ f x
smallLift _ _ = Nothing

-- Parser (Maybe (Text,Node)), Parser (Maybe StringComplex)
{-pcall = (\e a -> fromMaybe empty pure (smallLift (e<&>ProgramCall) (sequence a))) <$> exec <*> args
  where
    exec = capStr <$> (ws *> extractUntil blockers <* charP ' ' <* ws)
    args = many (capStr <$> (ws *> extractUntil blockers <* charP ' ' <* ws))
-}

capStr :: T.Text -> Maybe StringComplex
capStr t = f (runParser stringify t)
  where 
    f :: Maybe (T.Text, StringComplex) -> Maybe StringComplex 
    f t = case t of
      Just ("", x) -> Just x
      Just (r,  x) -> capStr r >>= \case
        Combination y -> Just $ Combination (x:y)
        y             -> Just $ Combination [x, y]
      Nothing -> Nothing --error "capStr died lmao"
stringify :: Parser StringComplex
stringify = basic <|> variant <|> envvar


envvar :: Parser StringComplex
envvar = EnvVar <$> (ws *> charP '$' *> extractUntil stringblockers <* ws)

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





data StringComplex = Basic T.Text | EnvVar T.Text | Variant [StringComplex] | Combination [StringComplex]
  deriving (Show,Eq)

complexToText :: StringComplex -> IO T.Text
complexToText (Basic t) = pure t
complexToText (EnvVar t) = getEnv (T.unpack t) >>= \case
                            Just x -> pure $ T.pack x
                            Nothing -> pure  T.empty
complexToText (Variant ts) = mapM complexToText ts <&> T.unwords
complexToText (Combination ts) = error $ show ts

complexToRawText :: StringComplex -> T.Text
complexToRawText (Basic t) = t
complexToRawText (EnvVar t) = "$" <> t
complexToRawText (Variant ts) = "{" <> T.intercalate "," (fmap complexToRawText ts) <> "}"


type Executable = StringComplex
type Args       = [StringComplex]

type FileName = (IO T.Text)

data PipeType = File FileName IOMode | Terminal

displayPipeType :: PipeType -> IO T.Text
displayPipeType Terminal = pure "Terminal"
displayPipeType (File fname mode) = fname >>= \x -> pure $ T.concat [x, "[", T.pack $ show mode, "]"]

displayHide :: PipeType -> T.Text
displayHide Terminal = "Terminal"
displayHide (File _ mode) = T.concat ["File ? ", T.pack $ show mode]

type Condition = (Int -> IO Bool)
data Task = Task {condition :: Condition, body :: Task, next :: Maybe Task, stdinT :: PipeType, stdoutT :: PipeType, stderrT :: PipeType} | PCall Executable Args

instance Show Task where
  show (PCall _ a) = "`" ++ "hidden behind IO"{-T.unpack (complexToText e)-} ++ " [" ++ (T.unpack . T.unwords) (fmap (const "hidden behind IO") a) ++ "]`"
  show (Task _ t n sin sout serr) = "{" ++ T.unpack (displayHide sin) ++ "}c -> " ++ show t ++ case n of
    Just x -> "=>" ++ show x
    Nothing -> ""
    ++ "-->" ++ T.unpack (displayHide sout)

displayTask :: Task -> IO T.Text
displayTask (Task c t n sin out serr) = case n of 
    Just x -> displayTask x >>= \y -> pure $ "=>" <> y
    Nothing -> pure "" 
  >>= \x -> displayTask t >>= \t -> displayPipeType sin >>= \sin -> displayPipeType out >>= \sout -> pure $ T.concat ["{", sin, "}c->", t, x, "-->", sout
  ]
displayTask (PCall e a) = mapM complexToText a >>= \as -> complexToText e >>= \es -> pure $ T.concat ["`", es, " [", T.unwords as, "]`"]



data CompletionRule = CompRule T.Text (T.Text -> IO [CompletionRule])

instance Show CompletionRule where
  show (CompRule x f) = "CompRule `" ++ T.unpack x ++ "`"


unwrapArgs :: CompletionRule -> [T.Text] -> IO [CompletionRule]
unwrapArgs (CompRule _ f) [t] = f t
unwrapArgs (CompRule _ f) (t:ts) = f t >>= \case
    [CompRule x f2] -> if x==t then unwrapArgs (CompRule x f2) ts else pure []
    _ -> pure []
-- todo: add completions and file cache to this
isValidArgument :: [CompletionRule] -> [T.Text] -> IO Bool
isValidArgument rules (executable:args) = case lookup' executable rules of
  Just (CompRule x f) -> unwrapArgs (CompRule x f) args <&> \case
    [CompRule x2 _] -> x2==head (reverse args)
    _   -> False
  Nothing             -> pure True
  where
    lookup' :: T.Text -> [CompletionRule] -> Maybe CompletionRule
    lookup' t (CompRule x f:xs) = bool (lookup' t xs) (Just $ CompRule x f) (t==x)
    lookup' _ [] = Nothing


-- STAMP

-- ansi stuff
langAsAnsi :: [CompletionRule] -> T.Text -> [T.Text] -> ColorScheme -> Int -> [T.Text] -> IO T.Text
langAsAnsi rules t whitespace colorScheme cursor executables = case runParser parseExpr t of
  Just (r, node) -> case node of
    (ProgramCall e a) -> do
      let exec = complexToRawText e
      let args = fmap complexToRawText a
      --zip wws args <- this is good I think
      --aargs <- 
      (<>) <$> (formatted exec <&> (<>w)) <*> formatArgs [exec] args wws colorScheme
      where
        formattedWord word e = (if e || T.null word then successColor colorScheme else errorColor colorScheme) <&> (<>word<>"\ESC[0m") . asciiColor
        formatted exec = findExecutable (T.unpack exec) >>= formattedWord exec . (|| exec `elem` executables) . isJust
        (w:wws) = whitespace++[""]

        -- work on this (!!). It breaks on  `whole`. Figure out why.
        formatArgs :: [T.Text] -> [T.Text] -> [T.Text] -> ColorScheme -> IO T.Text
        --formatArgs executable arguments whitespace cscheme = (<>) 
        formatArgs t' (a':as) (w':ws') cs = (<>) <$> n <*> (nt >>= \x -> formatArgs x as ws' cs)
          where
            n :: IO T.Text
            nt :: IO [T.Text]
            n = isValidArgument rules (t'++[a']) <&> (<>(a' <>"\ESC[0m"<> w')) . bool "\ESC[4m" ""
            nt = n <&> (t'++) . singleton
        formatArgs _ [] (w:ws) _ = pure w
        formatArgs _ [] [] _ = pure ""
        formatArgs _ x _ _ = pure ""
        --formatArgs _ _  _ _ = pure ""
    {-(And a b) -> do
      a_u <- nodeAsText a
      b_u <- langAsAnsi b whitespace' colorScheme cursor

      pure $ a_u <> w1 <> "&&" <> w2 <> b_u
      where
        whitespace' = undefined
        w1 = undefined
        w2 = undefined-}
    _ -> undefined
  Nothing -> (\x y z -> x<>y<>z) <$> (err <&> asciiColor) <*> pure t <*> pure "\ESC[0m"
    where err = errorColor colorScheme
