{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Language.Parser where

import qualified Data.Text as T
import Data.Functor
import Control.Applicative
import GHC.IO.IOMode
import Data.Char (isSpace)
import System.Posix (getEnv)

import System.Directory (doesDirectoryExist, getPermissions, Permissions (readable), getDirectoryContents)
import Data.Bool (bool)
import System.FilePath (takeDirectory, (</>))
import Control.Monad (filterM)
import Data.List (intersperse)

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

parseExpr, parseExpr', parseExpr'' :: Parser Node
parseExpr = andand <|> parseExpr'
parseExpr' = pipe <|> parseExpr''
parseExpr'' = pcall


-- parser for classic && operator

-- blockers are chars that symbolize a beggining of a new NODE, such as AND (`&&`). docs/parsing:1.2
blockers :: String
blockers = " &><|"

-- blockers that also block stringcomplex parsing
stringblockers :: String
stringblockers = blockers ++ " ${,}()"


pipe :: Parser Node
pipe = f Write (charP '>')
      <|> f Append (stringP ">>")
      <|> f WriteErr (stringP "2>")
      <|> f AppendErr (stringP "2>>")
  where
    f sx ssx = Pipe sx <$> (parseExpr'' <* ssx) <*> parseExpr

andand :: Parser Node
andand = And <$> (parseExpr' <* stringP "&&") <*> parseExpr

ws :: Parser T.Text
ws = spanP isSpace


spanPCount :: (Char -> Bool) -> Parser (T.Text, Int)
spanPCount f = Parser $ \i -> do
  (rest, txt) <- runParser (spanP f) i
  let n = T.length txt
  Just (rest, (txt, n))

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
      (i', program) <- runParser stringify i--second capStr <$> runParser (extractUntil " &") i
      (i'', _args) <- runParser extractArgs i'
      --program <- mprogram
      --_args <- sequence margs
      Just (i'', ProgramCall program _args)

extractArgs :: Parser [StringComplex]
extractArgs = many stringify


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
capStr s = f (runParser stringify s)
  where 
    f :: Maybe (T.Text, StringComplex) -> Maybe StringComplex 
    f t = case t of
      Just ("", x) -> Just x
      Just (r,  x) -> capStr r >>= \case
        (Combination y, ab) -> Just (Combination (x:y), ab)
        y             -> Just (Combination [x, y], ("",""))
      Nothing -> Nothing --error "capStr died lmao"
stringify :: Parser StringComplex
stringify = basic <|> variant <|> envvar

constructFrom :: (t -> a1) -> a2 -> t -> b -> (a1, (a2, b))
constructFrom t l c r = (t c, (l, r))

envvar :: Parser StringComplex
envvar = constructFrom EnvVar <$> ws <*> (charP '$' *> extractUntil stringblockers) <*> ws

basic :: Parser StringComplex
basic = constructFrom Basic <$> ws <*> extractUntil stringblockers <*> ws

variant :: Parser StringComplex
variant = constructFrom Variant <$> ws <*> (charP '{' *> variants <* charP '}') <*> ws
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
      | t == T.empty = Nothing
      | T.head t==x = Just (T.drop 1 t, x)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP



spanP :: (Char -> Bool) -> Parser T.Text
spanP f = Parser $ \input ->
  let (token, rest) = T.span f input
  in Just (rest, token)





data StringComplex' = Basic T.Text | EnvVar T.Text | Variant [StringComplex] | Combination [StringComplex]
  deriving (Show,Eq)

type StringComplex = (StringComplex', (T.Text, T.Text))

complexToText :: StringComplex -> IO T.Text
complexToText (Basic t, (a,b)) = pure $ a<>t<>b
complexToText (EnvVar t, (a,b)) = getEnv (T.unpack t) >>= \case
                            Just x -> pure $ a<>T.pack x<>b
                            Nothing -> pure $ a<>b
complexToText (Variant ts, (a,b)) = mapM complexToText ts <&> (<>b) . (a<>) . T.unwords
complexToText (Combination ts, (a,b)) = (\x -> a <> T.concat x <> b) <$> mapM complexToText ts


complexToText' :: StringComplex' -> IO T.Text
complexToText' (Basic t) = pure t
complexToText' (EnvVar t) = getEnv (T.unpack t) >>= \case
                            Just x -> pure $ T.pack x
                            Nothing -> pure ""
complexToText' (Variant ts) = mapM complexToText ts <&> T.unwords
complexToText' (Combination ts) = T.concat <$> mapM complexToText ts


complexToRawText :: StringComplex -> T.Text
complexToRawText (Basic t, (a,b)) = a<>t<>b
complexToRawText (EnvVar t, (a,b)) = a<>"$"<>t<>b
complexToRawText (Variant ts, (a,b)) = a<>"{" <> T.intercalate "," (fmap complexToRawText ts) <> "}"<>b
complexToRawText (Combination ts, (a,b)) = a<> T.concat (fmap complexToRawText ts) <> b

complexToRawText' :: StringComplex' -> T.Text
complexToRawText' (Basic t) = t
complexToRawText' (EnvVar t) = "$"<>t
complexToRawText' (Variant ts) = "{" <> T.intercalate "," (fmap complexToRawText ts) <> "}"
complexToRawText' (Combination ts) = T.concat (fmap complexToRawText ts)


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
  show (Task _ t n s_in s_out _s_err) = "{" ++ T.unpack (displayHide s_in) ++ "}c -> " ++ show t ++ case n of
    Just x -> "=>" ++ show x
    Nothing -> ""
    ++ "-->" ++ T.unpack (displayHide s_out)

displayTask :: Task -> IO T.Text
displayTask (Task _c t n s_in s_out _s_err) = case n of 
    Just x -> displayTask x >>= \y -> pure $ "=>" <> y
    Nothing -> pure "" 
  >>= \x -> displayTask t >>= \t2 -> displayPipeType s_in >>= \ss_in -> displayPipeType s_out >>= \sout -> pure $ T.concat ["{", ss_in, "}c->", t2, x, "-->", sout
  ]
displayTask (PCall e a) = mapM complexToText a >>= \as -> complexToText e >>= \es -> pure $ T.concat ["`", es, " [", T.unwords as, "]`"]



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
