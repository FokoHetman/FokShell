{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- | basic builtins
module FokShell.Builtin where
import FokShell.Types
import FokShell.Utils
import Language.Parser
import System.IO (stderr, openFile, hGetContents)
import Data.Text.IO qualified as T
import Data.Text qualified as T
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import System.Directory
import Data.Bool (bool)
import System.Posix
import Control.Concurrent
import Data.Functor
import Text.Regex.TDFA
import Data.Map qualified as Map
import FokShell.Module.TabCompletion
import Data.Data (Proxy(Proxy))
import FokShell.Module.DirectoryTracker (DirectoryTracker(DirectoryTracker, directories))
import Debug.Trace (traceShow)
import FokShell.Module (requestModule)
import Control.Concurrent.STM (modifyTVar, atomically, TVar, readTVarIO)


-- builtins {{{

cd, regex :: [T.Text] -> (TaskPipeType,TaskPipeType,TaskPipeType) -> TVar ShellConfig -> IO (ExitCode)
cd args (_inh, outh, errh) conf = do
    conf' <- readTVarIO conf
    errHandle <- case errh of
      Terminal -> pure stderr
      File fname mode -> openFile fname mode
      _ -> undefined
    let writeErr = T.hPutStr errHandle
    let updateDirTracker d = (mapM_ (atomically . (`modifyTVar`
          (\tracker -> tracker{directories=d:tracker.directories})))
          $ requestModule @DirectoryTracker conf'.modules)
    case args of
      [x] -> do
        let d = T.unpack x
        doesDirectoryExist d >>= bool 
          (writeErr ("cd: directory `" <> T.pack d <> "` does not exist.\n") $> (ExitFailure 1))
          ((searchable <$> getPermissions d) >>= bool
            (writeErr "cd: no permissions.\n" $> ExitFailure 1)
            (do
              changeWorkingDirectory d
              updateDirTracker =<< canonicalizePath d
              pure ExitSuccess))
      []  -> pure $ ExitSuccess
      _   -> writeErr "cd: too many args provided.\n" $> ExitFailure 2
{-set' args (inh, outh, errh) process = let
      f n = case outh of
          ProcessData oref -> putMVar oref (Left $ Set n) $> (ExitSuccess, process)
          Terminal -> displaySet n $> (ExitSuccess, process)
          File fname mode -> (openFile fname mode >>= (`T.hPutStr` (setToJson n))) $> (ExitSuccess, process)
    in case inh of
      ProcessData ref -> readMVar ref >>= \case
        Left n -> case withProxyNode (Proxy @SetExp)
        Right h -> do
          content <- hGetContents h
          case runParser jsonset $ T.pack content of
            Just (_,(Set n)) -> f n
            _ -> error "no parse"
        _ -> error "invalid argument"
      _ -> undefined

bmap' args (inh, outh, errh) process = case inh of
    ProcessData ref -> do
      let (name:argv) = args
      n <- readMVar ref
      case n of
        Left n -> do
          let ns = case n of
                Array ns' -> ns'
                Set ns' -> fmap snd $ Map.toList ns'
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
        Right _ -> error "map expects either an Array or a Set"
    _ -> undefined
-}
regex args (inh, outh, errh) process = case inh of
    ProcessData ref -> do
      a <- readMVar ref
      case a of
        Left n' -> do
          let (n,q) = case withProxyNode (Proxy @Primitive) n' of
                Just (NodeString x q) -> (x,q)
                --ProcessCall x _ -> nodeToString x
                _ -> error "invalid node provided"
          let arg = case args of
                  (x:_) -> x
                  _ -> ""
          let newt :: [String] = getAllTextMatches (T.unpack n =~ T.unpack arg)
          case outh of
            ProcessData ref' -> putMVar ref' . Left . Node . ArrayExp $ fmap (Node . (`NodeString` q) . T.pack) newt
            Terminal -> putStrLn $ unwords newt
            _ -> pure ()
          pure ExitSuccess
        Right h -> do
          content <- hGetContents h
          let arg = case args of
                  (x:_) -> x
                  _ -> ""
          let newt :: [String] = getAllTextMatches (content =~ T.unpack arg)
          case outh of
            ProcessData ref' -> putMVar ref' . Left . Node . ArrayExp $ fmap (Node . (`NodeString` SingleQuote) . T.pack) newt
            Terminal -> putStrLn $ unwords newt
            _ -> pure ()
          pure ExitSuccess
    _ -> do
      errHandle <- case errh of
        Terminal -> pure stderr
        File fname mode -> openFile fname mode
        _ -> undefined
      let writeErr = T.hPutStr errHandle
      writeErr "invalid argument"
      pure $ ExitFailure (-1)

-- }}}

-- completions {{{
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

-- }}}

-- vim: foldmethod=marker
