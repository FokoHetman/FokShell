{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Lib.Config where

import Lib.ColorScheme
import Lib.Primitive
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Bits as B
import Data.Dynamic (Dynamic, fromDynamic)
import Lib.Keys
import Lib.Format
import System.Process

import Data.ByteString qualified as BS


import System.Posix (fileExist, createFile, ownerWriteMode, ownerReadMode, closeFd, changeWorkingDirectory, getFileStatus, isDirectory, isRegularFile)
import System.Directory (getHomeDirectory, doesDirectoryExist, getPermissions, Permissions (readable, executable, searchable), getDirectoryContents, doesFileExist, doesPathExist)

import System.FilePath ((</>), takeDirectory)

import Debug.Trace

import Data.Functor

import GHC.IO.Handle
import Language.Parser
import GHC.IO.Exception (ExitCode (ExitSuccess, ExitFailure), IOException (IOError))
import Control.Monad
import System.Exit (exitSuccess)
import Data.List (sort, group, intersperse)
import Control.Concurrent (threadDelay, forkIO, MVar, newEmptyMVar, newMVar, readMVar, putMVar, isEmptyMVar)
import Data.Bool (bool)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text.IO qualified as T
import System.IO (openFile, IOMode (WriteMode, AppendMode), stdin, stderr)

import Data.Map qualified as Map

import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import FokShell.Module (Module (Module))
import Control.Exception (catch, throwIO)
import System.IO.Error (isEOFError)

exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess     = 0
exitCodeToInt (ExitFailure c) = c

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

mkTask' :: T.Text -> Maybe (IO Task)
mkTask' t = runParser parseSeq t <&> mkTask . snd

mkTask :: Node -> IO Task
mkTask (Sequence n1 n2) = do
  n2' <- mkTask n2
  n1' <- mkTask n1
  pure n2' {prevTask = Just n1', condition = const True}
mkTask (And n1 n2) = do
  n2' <- mkTask n2
  n1' <- mkTask n1
  pure $ n2' {prevTask = Just n1', condition = (==0) . exitCodeToInt}
mkTask (Pipe pt n1 n2) = case pt of
  ProcessPipe -> do
    n2' <- mkTask n2
    n1' <- mkTask n1
    ref2 <- newEmptyMVar
    pure n2' {pipeIn = ProcessData ref2, prevTask = Just $ n1' {pipeOut = ProcessData ref2}}
  Write Stdout -> mkFilePipe n1 n2 Stdout WriteMode
  Write Stderr -> mkFilePipe n1 n2 Stderr WriteMode
  Append Stdout -> mkFilePipe n1 n2 Stdout AppendMode
  Append Stderr -> mkFilePipe n1 n2 Stderr AppendMode
  Read -> undefined
  where
    mkFilePipe n1 n2 Stdout mode = do
      n1' <- mkTask n1
      pure n1' {pipeOut = File (T.unpack $ nodeToString n2) mode}
    mkFilePipe n1 n2 Stderr mode = do
      n1' <- mkTask n1
      pure n1' {pipeErr = File (T.unpack $ nodeToString n2) mode}
mkTask (ProcessCall (NodeString pname) args) = pure Task {
  procName = pname
, procArgs = fmap nodeToString args
, pipeIn = Terminal
, pipeOut = Terminal
, pipeErr = Terminal
, prevTask = Nothing
-- | given exit code of prevTask determines whether this task should run
, condition = const True
}
mkTask (Table t) = do
  h <- newMVar $ Left (Table t)
  pure Task {
    procName = "table"
  , procArgs = []
  , pipeIn = ProcessData h
  , pipeOut = ProcessData h
  , pipeErr = Terminal
  , prevTask = Nothing
  , condition = const True
  }
mkTask x = error $ "unknown task: " <> show x

{-
mkTask :: Node -> Task
mkTask (Sequence n1 n2) = BinaryTask {
  left = mkTask n1
, right = mkTask n2
, condition = const True
}
mkTask (And n1 n2) = BinaryTask {
  left = mkTask n1
, right = mkTask n2
, condition = (==0) . exitCodeToInt
}
mkTask (Pipe pt n1 n2) = case pt of
  ProcessPipe -> (mkTask n1) {pipeIn=NodePipe n2}
  Write Stdout -> (mkTask n1) {pipeOut=File (T.unpack $ (\(NodeString x) -> x) n2) WriteMode}
  Write Stderr -> (mkTask n1) {pipeErr=File (T.unpack $ (\(NodeString x) -> x) n2) WriteMode}
  Append Stdout -> (mkTask n1) {pipeOut=File (T.unpack $ (\(NodeString x) -> x) n2) AppendMode}
  Append Stderr -> (mkTask n1) {pipeErr=File (T.unpack $ (\(NodeString x) -> x) n2) AppendMode}
  Read -> undefined
mkTask (ProcessCall name args) = ProcessTask {
  procName = (\(NodeString x) -> x) name
, args = fmap (\(NodeString x) -> x) args
}

data Task = BinaryTask {
  left  :: Task
, right :: Task
-- | given exit code of left, determine whether right should be launched
, condition :: ExitCode -> Bool
} | ProcessTask {
  procName  :: T.Text
, args      :: [T.Text]
, pipeIn    :: TaskPipeType
, pipeOut   :: TaskPipeType
, pipeErr   :: TaskPipeType
}
-}
data Process = Process {
  pid       :: Pid
, procHandle:: ProcessHandle
, procOuth  :: Handle
, procErrh  :: Handle
, procInh   :: Handle
}

data Job = Job {
  task :: Task
}
{-
data Job = Job {
    pid     :: Maybe Pid
  --, task    :: Task
  , stdoutj :: Maybe Handle
  , stderrj :: Maybe Handle
  , stdinj  :: Maybe Handle
  , last_ec :: ExitCode
  , pipeOut :: PipeType
  , pipeIn  :: PipeType
  , pipeErr :: PipeType
}
-}

newtype JobMgr = JobMgr [Job]

{-
ESC[0 q 	changes cursor shape to steady block
ESC[1 q 	changes cursor shape to steady block also
ESC[2 q 	changes cursor shape to blinking block
ESC[3 q 	changes cursor shape to steady underline
ESC[4 q 	changes cursor shape to blinking underline
ESC[5 q 	changes cursor shape to steady bar
ESC[6 q 	changes cursor shape to blinking bar
-}

data CursorShape = SteadyBlock | BlinkingBlock | SteadyUnderline | BlinkingUnderline | SteadyBar | BlinkingBar

instance Show CursorShape where
  show BlinkingBlock      = "\ESC[0 q"
  show SteadyBlock        = "\ESC[2 q"
  show BlinkingUnderline  = "\ESC[3 q"
  show SteadyUnderline    = "\ESC[4 q"
  show BlinkingBar        = "\ESC[5 q"
  show SteadyBar          = "\ESC[6 q"


newtype CursorConfig = CursorConfig
  { cursorShape :: CursorShape
  }


data State = InputOutput

data ShellProcess = ShellProcess {
    shellConfig :: ShellConfig
  , shellState  :: State
  , shellCache  :: Cache T.Text (Cache T.Text Dynamic)
}


type Hook = ShellProcess -> IO Bool -- bool tells whether to continue afterhand action


type Action = ShellProcess -> IO ShellProcess


data ShellHooks = ShellHooks 
  { haltHook  :: Hook -- things to do before a HALT (^C)
  , exitHook  :: Hook -- things to do before exitting (^D, exit, etc)
  , startHook :: Hook -- like rc
  , clearHook :: Hook
  }


defaultHaltHook :: Hook 
defaultHaltHook _ = do
  putStrLn "^C"
  pure True

defaultHistoryFile :: IO FilePath
defaultHistoryFile = getHomeDirectory <&> (</> ".fok_history")
defaultExitHook :: Hook
defaultExitHook proc = do
  defaultHistoryFile >>= \x -> writeFile x $ T.unpack $ T.strip $ T.intercalate "\n" $ T.strip <$> reverse (history (shellConfig proc))
  putStrLn "\nexit"
  pure True

defaultClearHook :: Hook
defaultClearHook proc = do
  if lastEvent (shellConfig proc) == trigger (shellConfig proc) then
    pure False
  else
    pure True

defaultStartHook :: Hook
defaultStartHook _ = pure True

data Swallow = Never | Swallowed (IO T.Text)
data Prompt  = SingleLine (IO T.Text) Swallow | MultiLine (IO [T.Text]) Swallow

type PromptGetter = ColorScheme -> Prompt

type Builtin = (T.Text, [T.Text] -> (TaskPipeType, TaskPipeType, TaskPipeType) -> ShellProcess -> IO (ExitCode, ShellProcess))

cd :: Builtin
cd = ("cd", \args (_inh, outh, errh) process -> do
    errHandle <- case errh of
      Terminal -> pure stderr 
      File fname mode -> openFile fname mode
      _ -> undefined
    let writeErr = T.hPutStr errHandle
    _ <- case args of
      [x] -> do
        let d = T.unpack x
        doesDirectoryExist d >>= bool (writeErr "cd: directory does not exist.\n") (getPermissions d >>= bool (writeErr "cd: no permissions.\n") (changeWorkingDirectory d) . searchable)
      []  -> writeErr "cd: no arg provided.\n"
      _   -> writeErr "cd: too many args provided.\n"
    pure (ExitSuccess, process)  -- replace getters with just a cached value that changes here?
                                 --  ^^ what
  )

table :: Builtin
table = ("table", \args (inh, outh, errh) process -> let
      f n = case outh of
          ProcessData oref -> putMVar oref (Left $ Table n) $> (ExitSuccess, process)
          Terminal -> displayTable n $> (ExitSuccess, process)
          File fname mode -> (openFile fname mode >>= (`T.hPutStr` (tableToJson n))) $> (ExitSuccess, process)
    in case inh of
      ProcessData ref -> readMVar ref >>= \case
        Left (Table n) -> f n 
        Right h -> do
          content <- hGetContents h
          case runParser jsontable $ T.pack content of
            Just (_,(Table n)) -> f n
            _ -> error "no parse"
        _ -> error "invalid argument"
      _ -> undefined
      )

tableToJson :: (Map.Map Node Node) -> T.Text
tableToJson t = "{" <> T.concat (intersperse ",\n" $ fmap display' $ Map.toList t) <> "}"

displayTable :: (Map.Map Node Node) -> IO ()
displayTable t = T.putStrLn $ T.concat $ intersperse "\n" $ fmap display' $ Map.toList t

display' :: (Node, Node) -> T.Text
display' (n1, n2) = nodeToString n1 <> ": " <> nodeToString n2


safeGetChar :: Handle -> IO (Maybe Char)
safeGetChar h =
    (Just <$> hGetChar h)
    `catch` f
    where
      f :: IOError -> IO (Maybe Char)
      f = const $ pure Nothing

forward :: Handle -> Handle -> IO ()
forward read write = do
  char <- safeGetChar read
  case char of
    Just c -> do
      hPutChar write c
      forward read write
    Nothing -> pure ()

executeTask :: ShellProcess -> Task -> IO (ExitCode, ShellProcess)
executeTask proc' t = do
  let name = t.procName
  let args = t.procArgs
  let builtins = proc'.shellConfig.builtins
  case lookup name builtins of
    Just x  -> x args (t.pipeIn, t.pipeOut, t.pipeErr) proc'
    Nothing -> do
      outPipe <- getPipe t.pipeOut
      errPipe <- getPipe t.pipeErr
      inPipe <- getPipe t.pipeIn
      (inh, outh, errh, proch) <- createProcess (proc (T.unpack name) $ fmap T.unpack args) { std_out = outPipe, std_err = errPipe, std_in = inPipe }
      case t.pipeOut of
        ProcessData ref -> case outh of
          Just h -> putMVar ref $ Right h
          _ -> pure ()
        _ -> pure ()
      case t.pipeErr of
        ProcessData ref -> case errh of
          Just h -> putMVar ref $ Right h
          Nothing -> pure ()
        _ -> pure ()
      case t.pipeIn of
        ProcessData ref -> readMVar ref >>= \case
            Left n -> case inh of
              Just inh' -> hPutStr inh' (T.unpack $ nodeToString n) >> hFlush inh' >> hClose inh'
              Nothing -> pure ()
            _ -> pure ()
        _ -> pure ()
      exitCode <- waitForProcess proch
      pure (exitCode, proc')
  where
    getPipe :: TaskPipeType -> IO StdStream
    getPipe (ProcessData ref) = isEmptyMVar ref >>= bool (readMVar ref <&> \case
      Left _ -> CreatePipe
      Right h -> UseHandle h
      )
      (pure CreatePipe)
    getPipe (Terminal) = pure Inherit
    getPipe (File f m) = openFile f m <&> UseHandle


bmap :: Builtin
bmap = ("map", \args (inh, outh, errh) process -> case inh of
    ProcessData ref -> do
      let (name:argv) = args
      n <- readMVar ref
      case n of
        Left n -> do
          let ns = case n of
                Array ns' -> ns'
                Table ns' -> fmap snd $ Map.toList ns'
                _ -> undefined
          let defaultTask = Task {
            procName = name
          , procArgs = argv
          , pipeIn = Terminal
          , pipeOut = Terminal
          , pipeErr = Terminal
          , prevTask = Nothing
          , condition = const True
          }
          tasks <- mapM (\x -> do
            y' <- newMVar $ Left x
            pure $ defaultTask {pipeIn = ProcessData y'}
            ) ns
          -- TODO: collect out into whatever `n` is and push into outh
          mapM_ (executeTask process) tasks
          pure (ExitSuccess, process)
        Right _ -> error "map expects either an Array or a Table"
    _ -> undefined
    )

regex :: Builtin
regex = ("regex", \args (inh, outh, errh) process -> case inh of
    ProcessData ref -> do
      a <- readMVar ref
      case a of
        Left n' -> do
          let n = case n' of
                NodeString x -> x
                ProcessCall x _ -> nodeToString x
                _ -> error "invalid node provided"
          let arg = case args of
                  (x:_) -> x
                  _ -> ""
          let newt :: [String] = getAllTextMatches (T.unpack n =~ T.unpack arg)
          case outh of
            ProcessData ref' -> putMVar ref' . Left . Array $ fmap (NodeString . T.pack) newt
            Terminal -> putStrLn $ unwords newt
            _ -> pure ()
          pure (ExitSuccess, process)
        Right h -> do
          content <- hGetContents h
          let arg = case args of
                  (x:_) -> x
                  _ -> ""
          let newt :: [String] = getAllTextMatches (content =~ T.unpack arg)
          case outh of
            ProcessData ref' -> putMVar ref' . Left . Array $ fmap (NodeString . T.pack) newt
            Terminal -> putStrLn $ unwords newt
            _ -> pure ()
          pure (ExitSuccess, process)
    _ -> do
      errHandle <- case errh of
        Terminal -> pure stderr
        File fname mode -> openFile fname mode
        _ -> undefined
      let writeErr = T.hPutStr errHandle
      writeErr "invalid argument"
      pure (ExitFailure $ -1, process)
  )

animateMovement :: [String] -> IO ()
animateMovement x = putStrLn "" >> f x (length (head x))
  where
    f :: [String] -> Int -> IO ()
    f x 0 = putStrf' ("\ESC[" ++ show (length x) ++ "B")
    f x i = f' x i >> putStrf' ("\ESC[" ++ show (length x) ++ "A") >> threadDelay 50000 >> f x (i-1)
    f' :: [String] -> Int -> IO ()
    f' (x:xs) i = putStrLn (reverse $ take (length x - i) $ reverse x) >> f' xs i
    f' [] _ = pure ()
count :: Eq a => [a] -> a -> Int
count xs find = length (filter (== find) xs)

dedup :: Ord a => [a] -> [a]
dedup = map head . group . sort

{-
wrapped :: Builtin
wrapped = ("wrapped", \_ process -> do
    let conf = shellConfig process
    let hist = history conf
    let mis_ls = count hist "sl"
    let mis_nvim = sum $ fmap (count hist . T.pack) $ filter (/="nvim") $ dedup [a:b:c:[d] | a<-"nvim", b<-"nvim", c<-"nvim", d<-"nvim"]
    
    
    animateMovement [
        "\ESC" ++ concat (intersperse "*====" ["\ESC[38;5;" ++ show id ++ "m" | id<- [0..20]]) ++ "*\ESC[0m" --"*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*"
      ,  "                                                          \ESC[1m==============*FokShell Wrapped*=====\ESC[0m"
      ,  "                                      \ESC[1mMisspelled `ls` " ++ show mis_ls ++ " times\ESC[0m"
      ,  "               \ESC[1mMisspelled `nvim` " ++ show mis_nvim ++ " times\ESC[0m"
      , "\ESC" ++ concat (intersperse "*====" ["\ESC[38;5;" ++ show id ++ "m" | id<- [0..20]]) ++ "*\ESC[0m"
      --, "*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*"
      , "\xE001\xE002\xE003\xE004\xE005\xE006\xE007\xE008\xE009\xE00A\xE00B\xE00C\xE00D\xE00E\xE00F\xE010\xE011\xE012\xE013\xE014\xE015\xE016\xE017\xE018\xE019\xE01A\xE01B\xE01C\xE01D\xE01E\xE01F\xE020\xE021\xE022\xE023\xE024\xE025\xE026\xE027\xE028\xE029\xE02A\xE02B\xE02C\xE02D\xE02E\xE02F\xE030\xE031\xE032\xE033\xE034\xE035\xE036\xE037\xE038\xE039\xE03A\xE03B\xE03C\xE03D\xE03E\xE03F\xE040\xE041\xE042\xE043\xE044\xE045\xE046\xE047\xE048\xE049\xE04A\xE04B\xE04C\xE04D\xE04E\xE04F\xE050\xE051\xE052\xE053\xE054\xE055\xE056\xE057\xE058\xE059\xE05A\xE05B\xE05C\xE05D\xE05E\xE05F\xE060"
      ]

    pure process
  )
-}

readHistory :: IO FilePath -> IO [T.Text]
readHistory f2 = f2 >>= (\f -> fileExist f >>= \x -> unless x (void $ trace ("creating a history file: `" ++ f ++ "`") $ createFile f (ownerReadMode B..|. ownerWriteMode) >>= closeFd) >> TIO.readFile f <&> reverse . T.split (=='\n'))


haltAction :: Action
haltAction proc = let config = shellConfig proc in displayPrompt (prompt config  $ colorScheme config) $> proc {shellConfig = config {input = "",cursorLoc=0}}

exitAction :: Action
exitAction (ShellProcess {}) = exitSuccess

-- BUG: it doesn't display the current input, making clear with prompt yield weird results
clearAction :: Action
clearAction proc = let config = shellConfig proc; d = (T.length (input config) - cursorLoc config) in putStrLn "\ESC[2J\ESC[H" *> displayPrompt (prompt config $ colorScheme config) >> 
                   when (d>0) (moveCursor DRight d) {->> autocompleteRedraw proc-} $> proc

displayPrompt :: Prompt -> IO ()
displayPrompt = \case 
  SingleLine text _ -> eputStrf text
  MultiLine text _  -> eputStrf $ text <&> T.intercalate "\n"




{-nix :: CompletionRule
nix = CompRule "nix" (\t -> pure $ filter (\(CompRule i _) -> t `T.isPrefixOf` i) [
    CompRule "run" flake
  ])
  where
    flake :: T.Text -> IO [CompletionRule]
    flake t' = bool (matchFlake t') (matchAttr t') (T.elem '#' t')
      where
      matchFlake = (++) <$> fileCompletion ((<&> isDirectory) . getFileStatus) $ const $ pure [] <*> registries
      matchAttr t = case T.split (=='#') t of
        [_,x] -> (++) <$> directoryRules <*> registries
        _ -> undefined
-}

cdCompletion :: CompletionRule
cdCompletion = CompRule "cd" $ fileCompletion ((<&> isDirectory) . getFileStatus) $ const (pure [])


data ShellConfig = ShellConfig
  { hooks       :: ShellHooks
  , prompt      :: PromptGetter
  
  , colorScheme :: ColorScheme

  , cursorLoc   :: Int                  -- from the right, surprisingly
--, cursor      :: CursorConfig
  , input       :: T.Text
  , binds       :: [(KeyEvent, Action)]
  , lastEvent   :: KeyEvent
  , trigger     :: KeyEvent             -- this should never be overriden globally, locally it should be overwritten with the keyevent trigger (example at ^L handling)
  , jobManager  :: JobMgr

  -- todo: extract into a separate Object, just like ColorSchemes and Autocomplete. Add settings such as ignore duplicates etc.
  , history     :: [T.Text]
  , historyIndex:: Maybe (Int, T.Text)
  , getHistory  :: IO [T.Text]
  
  , builtins    :: [Builtin]

  --, autocomplete:: AutocompleteConfig
  , cursorConfig:: CursorConfig

  , completionRules :: [CompletionRule]

  , modules :: [Module ShellProcess]
  }

executablelist :: ShellProcess -> [T.Text]
executablelist proc = maybe [] (fromMaybe [] . fromDynamic) (lookupCache (shellCache proc) "executables" >>= \x -> lookupCache x "execs")

{-
mdata :: ShellProcess -> AutocompleteModelData
mdata proc = let c = shellConfig proc in AutocompleteModelData {modelInput = input c, aColorScheme = colorScheme c, cursorLocation = cursorLoc c, 
              historyL = history c, executableList = executablelist proc, builtinNames = fmap fst (builtins c), 
              modelOutput = ([],[]), mCompletionRules = completionRules c}
-}
--autocompleteRedraw proc' = let c = shellConfig proc' in model (autocomplete c) (mdata proc') >>= \x -> forceRedraw (autocomplete c) $ (mdata proc') {modelOutput = x}
