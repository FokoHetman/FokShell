{-# LANGUAGE OverloadedStrings,LambdaCase #-}
module JobManager where

import qualified Data.Text as T
import ExposedTypes
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Data.Functor
import Debug.Trace (trace, traceShow)
import Control.Arrow (Arrow(second))
import System.Process (CreateProcess(std_out, std_in, std_err), createProcess, proc, StdStream (CreatePipe, UseHandle, Inherit), getPid, waitForProcess)
import GHC.IO.Exception (ExitCode(ExitSuccess))

import System.IO

--import System.Process (CreateProcess (std_out))

-- implementing Call:
-- create a function: call :: T.Text (prog) -> [T.Text] (args) -> PID 
-- job manage that.



handleJob :: ShellProcess -> IO ShellProcess
handleJob (ShellProcess conf state) = do
  --putStrLn $ "Job Manager handling: " ++ T.unpack (input conf)
  let task = mkTask $ T.strip $ input conf

  
  --(Just stdin, Just std_out, Just stderr, proc_handle) <- createProcess (proc "sudo" ["-iu"]) { std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe }

  case task of
    -- overriding input here
    Just t  -> displayTask t >>= print >> spawnJob conf { input="", cursorLoc=0 } (mkJob t Terminal Terminal Terminal) >>= \x -> pure $ ShellProcess x state
    Nothing -> pure $ ShellProcess conf state

  --evaluateNodes (ShellProcess conf state) unodes
  --pure (ShellProcess conf state)


spawnJob :: ShellConfig -> Job -> IO ShellConfig
spawnJob conf j = do
  case task j of
    Task c b n sin sout serr -> do
      execute <- c $ exitCodeToInt $ last_ec j
      evaluatedConf <- if execute then spawnJob conf $ mkJob b sin sout serr else undefined
      case n of 
        Just n2 -> spawnJob evaluatedConf $ mkJob n2 sin sout serr -- or Terminal Terminal, idfk
        Nothing -> pure evaluatedConf
    PCall n a  -> do
      --if isBuiltin n then executeBuiltin n a else
      stdoutr <- getHandle pipeOut
      stdinr  <- getHandle pipeIn
      stderrr <- getHandle pipeErr

      pname <- complexToText n
      args  <- mapM complexToText a
      (s_in,sout,serr,ph) <- createProcess (proc (T.unpack pname) $ fmap T.unpack args)
          {std_out = stdoutr, std_in = stdinr, std_err = stderrr}
      let (JobMgr jobs) = jobManager conf
      let newjob = j {stdinj = s_in, stdoutj = sout, stderrj = serr}

      -- use process handle instead of pid in Job.
      getPid ph >>= \case
        Just p -> do
          exitcode <- waitForProcess ph
          pure conf {jobManager = JobMgr $ (newjob {pid = Just p, last_ec = exitcode}):jobs}
        Nothing -> undefined -- undefined behavior. idk what to do when process has no id
  --(t', h) <- walkTask $ task j
  where
    getHandle :: (Job -> PipeType) -> IO StdStream
    getHandle fun = case fun j of
        Terminal -> pure Inherit
        File f m -> f >>= \uf -> openFile (T.unpack uf) m >>= \x -> pure $ UseHandle x


--walkTask :: Task -> IO (Task, Bool)
--walkTask (PCall e a) = Nothing

mkJob :: Task -> PipeType -> PipeType -> PipeType -> Job
mkJob t i o e = Job {
    pid       = Nothing
  , task      = t
  , stdinj    = Nothing
  , stdoutj   = Nothing
  , stderrj   = Nothing
  , last_ec   = ExitSuccess

  , pipeIn    = i
  , pipeOut   = o
  , pipeErr   = e
  }

mkTask :: T.Text -> Maybe Task
mkTask t = do
  (_, n) <- runParser parseExpr t
  taskify n


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
