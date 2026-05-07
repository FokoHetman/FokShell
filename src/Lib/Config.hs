{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Lib.Config where

import Lib.ColorScheme
import Lib.Primitive
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Bits as B
import Data.Dynamic (Dynamic, fromDynamic, Typeable)
import Lib.Keys
import Lib.Format
import System.Process
import System.Posix (fileExist, createFile, ownerWriteMode, ownerReadMode, closeFd, changeWorkingDirectory, getFileStatus, isDirectory, isRegularFile)
import System.Directory (getHomeDirectory, doesDirectoryExist, getPermissions, Permissions (readable, executable, searchable), getDirectoryContents, doesFileExist, doesPathExist)
import System.FilePath ((</>))
import Debug.Trace
import Data.Functor
import GHC.IO.Handle
import Language.Parser
import GHC.IO.Exception (ExitCode (ExitSuccess, ExitFailure), IOException (IOError), IOErrorType (NoSuchThing, PermissionDenied))
import Control.Monad
import System.Exit (exitSuccess)
import Data.List (sort, group, intersperse)
import Control.Concurrent (threadDelay, forkIO, MVar, newEmptyMVar, newMVar, readMVar, putMVar, isEmptyMVar)
import Data.Bool (bool)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text.IO qualified as T
import System.IO (openFile, IOMode (WriteMode, AppendMode), stdin, stderr, stdout)
import Data.Map qualified as Map
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import FokShell.Module qualified as Module
import Control.Exception (catch)
import System.IO.Error (isEOFError, isDoesNotExistError, ioeGetErrorType, ioeGetErrorString, ioeGetLocation, ioeGetFileName)
import Data.Data (Proxy)


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
mkTask (ProcessCall (NodeString pname _) args) = pure Task {
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

data Process = Process {
  pid       :: Pid
, procHandle:: ProcessHandle
, procOuth  :: Handle
, procErrh  :: Handle
, procInh   :: Handle
}

data Job = Job {
  task :: Task
, exitCode :: MVar ExitCode
}

data State = InputOutput

data ShellProcess = ShellProcess {
    shellConfig :: ShellConfig
  , shellState  :: State
  , shellCache  :: Cache T.Text (Cache T.Text Dynamic)
}
modifyModule' :: forall a. (Module.Module' a ShellProcess,Typeable a) => Proxy a -> ShellProcess -> (a -> a) -> ShellProcess
modifyModule' p proc f = proc {shellConfig = proc.shellConfig {modules = Module.modifyModule p proc.shellConfig.modules f}}


type Hook = ShellProcess -> IO Bool -- bool tells whether to continue afterhand action


type Action = ShellProcess -> IO ShellProcess

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

handleProcessException :: ShellProcess -> IOError -> IO (ExitCode, ShellProcess)
handleProcessException proc' e = do
  traceIO $ fromMaybe "" ((<>": ") <$> ioeGetFileName e) <> ioeGetErrorString e
  let ecode = case ioeGetErrorType e of
            NoSuchThing      -> ExitFailure 127
            PermissionDenied -> ExitFailure 126
            _                -> ExitFailure 1
  pure (ecode, proc')
executeTask :: ShellProcess -> Task -> IO (ExitCode, ShellProcess)
executeTask proc' t = do
  let name = t.procName
  let args = t.procArgs
  let builtins = proc'.shellConfig.builtins
  case lookup name builtins of
    Just x  -> x args (t.pipeIn, t.pipeOut, t.pipeErr) proc'
    Nothing -> do
      (`catch` handleProcessException proc') $ do
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
                NodeString x _ -> x
                ProcessCall x _ -> nodeToString x
                _ -> error "invalid node provided"
          let arg = case args of
                  (x:_) -> x
                  _ -> ""
          let newt :: [String] = getAllTextMatches (T.unpack n =~ T.unpack arg)
          case outh of
            ProcessData ref' -> putMVar ref' . Left . Array $ fmap ((`NodeString` True) . T.pack) newt
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
            ProcessData ref' -> putMVar ref' . Left . Array $ fmap ((`NodeString` True) . T.pack) newt
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
-}

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
cdCompletion = CompRule "cd" $ fileCompletion (safeCheck $ (<&> isDirectory) . getFileStatus) $ const (pure [])

safeCheck :: (FilePath -> IO Bool) -> FilePath -> IO Bool
safeCheck f p = do
  b1 <- doesFileExist p
  b2 <- doesDirectoryExist p

  bool (pure True) (f p) (b1 || b2)


data ShellConfig = ShellConfig
  { cursorLoc   :: Int                  -- from the right, surprisingly
  , input       :: T.Text
  , binds       :: [(KeyEvent, Action)]
  , lastEvent   :: KeyEvent
  , trigger     :: KeyEvent             -- this should never be overriden globally, locally it should be overwritten with the keyevent trigger (example at ^L handling)
  , builtins    :: [Builtin]
  , completionRules :: [CompletionRule]
  , modules :: [Module.Module ShellProcess]
  }

executablelist :: ShellProcess -> [T.Text]
executablelist proc = maybe [] (fromMaybe [] . fromDynamic) (lookupCache (shellCache proc) "executables" >>= \x -> lookupCache x "execs")
