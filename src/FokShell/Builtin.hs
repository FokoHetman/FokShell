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


-- builtins {{{
table, cd, bmap, regex :: Builtin
table = ("table", table')
cd = ("cd", cd')
bmap = ("map", bmap')
regex = ("regex", regex')


cd', table', bmap', regex' :: [T.Text] -> (TaskPipeType,TaskPipeType,TaskPipeType) -> ShellProcess -> IO (ExitCode,ShellProcess)
cd' args (_inh, outh, errh) process = do
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
    pure (ExitSuccess, process)
table' args (inh, outh, errh) process = let
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
bmap' args (inh, outh, errh) process = case inh of
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
regex' args (inh, outh, errh) process = case inh of
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

cdCompletion :: CompletionRule
cdCompletion = CompRule "cd" $ fileCompletion (safeCheck $ (<&> isDirectory) . getFileStatus) $ const (pure [])
-- }}}

-- vim: foldmethod=marker
