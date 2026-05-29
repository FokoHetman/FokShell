{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.History where

import Data.Bits qualified as B
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Functor ((<&>))
import Data.Bool
import Data.Map qualified as Map
import System.FilePath ((</>), takeDirectory)

import Lib.Primitive
import System.Directory (getHomeDirectory, doesFileExist, createDirectoryIfMissing)
import FokShell.Module
import FokShell.Types
import Lib.Keys
import Control.Monad (unless, when)
import System.Posix (createFile, ownerReadMode, ownerWriteMode, closeFd)
import Lib.Format

import Data.Functor

import FokShell.Utils (head', moveCursor', redrawFromCursor)

import Control.Concurrent.STM
import System.IO (hFlush, stdout)

data History = History
  {
    history     :: [T.Text]
  , historyIndex:: Maybe (Int, T.Text)
  , getHistory  :: IO [T.Text]
  , appendToHistory :: T.Text -> [T.Text] ->  [T.Text]
  , writeHistory:: [T.Text] -> IO ()
  , entryLimit  :: Int
  , addBuiltins :: Bool
  }

instance Def History where
  def = historyFile (withHomeDir ".config/fokshell/history") 10000

withHomeDir :: FilePath -> IO FilePath
withHomeDir p = getHomeDirectory <&> (</> p)

historyFile :: IO FilePath -> Int -> History
historyFile path limit = History {
    history = []
  , historyIndex = Nothing
  , getHistory = do
    path' <- path
    doesFileExist path' >>= (`unless` do
      createDirectoryIfMissing True $ takeDirectory path'
      createFile path' (ownerReadMode B..|. ownerWriteMode) >>= closeFd
      )
    reverse . T.split (=='\n') <$> T.readFile path'
  , writeHistory = \history -> (path >>= (`T.writeFile` T.intercalate "\n" (reverse history)))
  , appendToHistory = \x' xs -> let x = T.strip x' in bool id (x:) (not (T.null x) && Just x /= head' xs) xs
  , entryLimit = limit
  , addBuiltins = False
  }

historyBuiltin :: Builtin
historyBuiltin = undefined

instance Module' History ShellConfig where
  initHook' m p = do
    m' <- atomically (readTVar m)
    history <- m'.getHistory
    atomically . modifyTVar m $ \m'' -> m'' {history=history}
    m' <- atomically (readTVar m)
    atomically . modifyTVar p $ \p' -> p' {builtins = bool id (Map.insert "history" undefined) m'.addBuiltins $ p'.builtins}
    pure () --(m {history = history}, p {shellConfig = p.shellConfig {builtins = bool id (historyBuiltin:) m.addBuiltins $ p.shellConfig.builtins}})
  exitHook' m _p = do
    m' <- readTVarIO m
    m'.writeHistory (take m'.entryLimit m'.history)
  resetHook' m _p = (atomically . modifyTVar m $ \m' -> m' {historyIndex = Nothing}) $> True
  preHook' m p (KeyModifiers 0, Enter) = do
    p' <- readTVarIO p
    atomically (modifyTVar m $ \m' -> m' {history=m'.appendToHistory p'.input m'.history, historyIndex = Nothing})
    pure True
  preHook' m p (KeyModifiers 0, Arrow Up) = do
    conf <- readTVarIO p
    when (T.length conf.input - conf.cursorLoc > 0) (moveCursor' p DLeft $ T.length conf.input - conf.cursorLoc)
    m' <- readTVarIO m
    let (m'',conf') = case m'.historyIndex of
          Nothing -> case m'.history of 
            []      -> (m', conf)
            (x:_)  -> (m' {historyIndex = Just (0, input conf)}, conf {input = x})
          Just (i, r) -> let j = min (length m'.history - 1) (i+1) in (m' {historyIndex = Just (j, r)}, conf {input = m'.history!!j})
    atomically $ writeTVar m m''
    atomically $ writeTVar p conf' {cursorLoc = 0}
    redrawFromCursor conf' {cursorLoc = T.length $ conf'.input}
    moveCursor DRight (T.length conf'.input) >> hFlush stdout
    pure True
  preHook' m p (KeyModifiers 0, Arrow Down) = do
    conf <- readTVarIO p
    when (T.length conf.input - conf.cursorLoc > 0 ) (moveCursor' p DLeft (T.length (input conf) - cursorLoc conf))
    m' <- readTVarIO m
    let (m'',conf') = case m'.historyIndex of
          Nothing -> (m', conf)
          Just (0,r) -> (m' {historyIndex=Nothing},conf {input=r})
          Just (i, r) -> let j = max 0 (i-1) in (m' {historyIndex = Just (j, r)}, conf {input = m'.history!!j})
    atomically $ writeTVar m m''
    atomically $ writeTVar p conf' {cursorLoc = 0}
    redrawFromCursor conf' {cursorLoc = T.length $ conf'.input}
    moveCursor DRight $ T.length $ conf'.input
    pure True
  preHook' _m _p _ = pure True
  postHook' _m _p _ = pure True
