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
import System.Process (CreateProcess(std_out, std_in, std_err), createProcess, proc, StdStream (CreatePipe), getPid, waitForProcess)
import GHC.IO.Exception (ExitCode(ExitSuccess))

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
    Just t  -> print (displayTask t) >> spawnJob conf { input="" } (mkJob t) >>= \x -> pure $ ShellProcess x state
    Nothing -> pure $ ShellProcess conf state

  --evaluateNodes (ShellProcess conf state) unodes
  --pure (ShellProcess conf state)


spawnJob :: ShellConfig -> Job -> IO ShellConfig
spawnJob conf j = do
  case task j of
    Task c b n -> do
      execute <- c $ exitCodeToInt $ last_ec j
      evaluatedConf <- if execute then spawnJob conf $ mkJob b else undefined
      case n of 
        Just n2 -> spawnJob evaluatedConf $ mkJob n2
        Nothing -> pure evaluatedConf
    PCall n a  -> do
      --if isBuiltin n then executeBuiltin n a else
      (sin,sout,serr,ph) <- createProcess (proc (T.unpack $ complexToText n) $ fmap (T.unpack . complexToText) a)  -- Task should specify whether it's hooked (attached to initial stdin) or not (background task). So basically pipes lmao.
      let (JobMgr jobs) = jobManager conf
      let newjob = j {stdinj = sin, stdoutj = sout, stderrj = serr}

      -- use process handle instead of pid in Job.
      getPid ph >>= \case
        Just p -> do 
          exitcode <- waitForProcess ph
          pure conf {jobManager = JobMgr $ (newjob {pid = Just p, last_ec = exitcode}):jobs}
        Nothing -> undefined -- undefined behavior. idk what to do when process has no id
  --(t', h) <- walkTask $ task j


--walkTask :: Task -> IO (Task, Bool)
--walkTask (PCall e a) = Nothing

mkJob :: Task -> Job
mkJob t = Job {
    pid       = Nothing
  , task      = t
  , stdinj    = Nothing
  , stdoutj   = Nothing
  , stderrj   = Nothing
  , last_ec   = ExitSuccess
  }

mkTask :: T.Text -> Maybe Task
mkTask t = do
  (_, n) <- runParser parseExpr t
  Just $ taskify n


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

taskify :: Node -> Task
taskify (And n1 n2) = Task (\_ -> pure True) (taskify n1) (Just $ Task (\e -> pure $ e==0) (taskify n2) Nothing)
taskify (ProgramCall e a) = PCall e a
taskify x = error $ "couldn't taskify: " ++ show x

parseExpr, parseExpr' :: Parser Node
parseExpr = andand <|> parseExpr'
parseExpr' = pcall



-- parser for classic && operator

-- blockers are chars that symbolize a beggining of a new NODE, such as AND (`&&`). docs/parsing:1.2 (todo)
blockers :: String
blockers = " $&><|"

-- blockers that also block stringcomplex parsing
stringblockers :: String
stringblockers = blockers ++ " {,}()"


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
