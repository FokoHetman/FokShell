{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module FokShell.Utils where
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (when, filterM)
import System.Directory (getCurrentDirectory, getDirectoryContents, getPermissions, Permissions (executable), doesFileExist, canonicalizePath)
import System.Posix (getEnv)
import System.FilePath (takeFileName)
import Data.Dynamic (toDyn)
import Data.Functor
import Lib.Primitive
import Lib.Format
import Lib.Keys
import FokShell.Types
import GHC.IO.Exception (ExitCode (ExitSuccess, ExitFailure))
import Language.Parser
import System.IO (openFile)
import Control.Concurrent (isEmptyMVar, putMVar, swapMVar, readMVar)

import Data.Map qualified as Map
import Data.List (intersperse)
import Data.Bool (bool)
import Control.Concurrent.STM


writeText :: T.Text -> TaskPipeType -> IO ()
writeText t Terminal = T.putStr t
writeText t (File path mode) = openFile path mode >>= (`T.hPutStr` t)
writeText t (ProcessData m) = isEmptyMVar m >>= bool
  (readMVar m >>= \case
    Left _ -> void . swapMVar m . Left . Node $ NodeString t SingleQuote
    Right h -> T.hPutStr h t
  )
  (putMVar m . Left . Node $ NodeString t SingleQuote)

updatePath :: ShellProcess -> IO ShellProcess 
updatePath proc = do
  path <- getEnv "PATH" >>= \case
    Just x -> pure x
    _ -> pure ""
  localFiles <- getDirectoryContents =<< getCurrentDirectory
  localExecusets <- fmap ("./" <>) <$> (mapM canonicalizePath localFiles >>= filterM (safeCheck (fmap executable . getPermissions)))
  pathExecs <- (pathExecusets >>= mapM canonicalizePath . concat)  <&> fmap takeFileName
  let envcache = Entry ("executables", Cache [Entry ("PATH", toDyn path), Entry ("execs", toDyn $ fmap T.pack $ pathExecs ++ localExecusets)])

  pure proc {shellCache = Cache $ getCache (removeFromCache (shellCache proc) "executables") ++ [envcache]}
  
  where
    pathExecusets = mapM executablesInDir =<< getDirsInPath

exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess     = 0
exitCodeToInt (ExitFailure c) = c

clearLines :: Direction -> Int -> IO ()
clearLines _ 0 = putStr "\ESC[2K\r"
clearLines d i = putStr "\ESC[2K\r" >> moveCursor d 1 >> clearLines d (i-1)

redrawFromCursor :: ShellConfig -> IO ()
redrawFromCursor c = putStrf $ T.concat [erase, lefts, cursorCode]
  where
    erase = T.pack "\ESC[0K"
    lefts = T.reverse $ T.take (cursorLoc c) (T.reverse $ input c)
    cursorCode = if T.length lefts > 0 then T.concat ["\ESC[", T.pack $ show $ T.length lefts, "D"] else T.empty

moveCursor' :: TVar ShellConfig -> Direction -> Int -> IO ()
moveCursor' c' DLeft  i = do
  c <- atomically $ readTVar c'
  when (T.length (c.input) - i >= c.cursorLoc) $ do
    atomically $ modifyTVar c' $ \x -> x {cursorLoc = c.cursorLoc + i}
    moveCursor DLeft i
moveCursor' c' DRight i = do
  c <- atomically $ readTVar c'
  when (c.cursorLoc >= i) $ do
    atomically $ modifyTVar c' $ \x -> x {cursorLoc = c.cursorLoc - i}
    moveCursor DRight i
moveCursor' _ _ _ = error "unsupported '-wrapped direction"

{-
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
mkTask (Set t) = do
  h <- newMVar $ Left (Set t)
  pure Task {
    procName = "set"
  , procArgs = []
  , pipeIn = ProcessData h
  , pipeOut = ProcessData h
  , pipeErr = Terminal
  , prevTask = Nothing
  , condition = const True
  }
mkTask x = error $ "unknown task: " <> show x
-}

{-modifyModule' :: forall a. (Module.Module' a ShellConfig,Typeable a) => Proxy a -> ShellConfig -> (a -> a) -> ShellConfig
modifyModule' p conf f = conf {modules = m} where m = Module.modifyModule p conf.modules f
-}
updateWithKey :: KeyEvent -> ShellConfig -> ShellConfig
updateWithKey event config = config {lastEvent=event}

setToJson :: (Map.Map Node Node) -> T.Text
setToJson t = "{" <> T.concat (intersperse ",\n" $ fmap display' $ Map.toList t) <> "}"

displaySet :: (Map.Map Node Node) -> IO ()
displaySet t = T.putStrLn $ T.concat $ intersperse "\n" $ fmap display' $ Map.toList t

display' :: (Node, Node) -> T.Text
display' (n1, n2) = nodeToText n1 <> ": " <> nodeToText n2

head' [] = Nothing
head' (x:_) = Just x
tail' [] = Nothing
tail' (_:xs) = Just xs


bool' a b c = bool b c a
