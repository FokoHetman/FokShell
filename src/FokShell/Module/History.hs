{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module FokShell.Module.History where

import Data.Bits qualified as B
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Bool
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

import FokShell.Utils

import Control.Concurrent.STM
import System.IO (hFlush, stdout, stdin)
import Control.Monad.IO.Class (MonadIO(liftIO))
import FokShell.Module.Input

data HistoryConfig = HistoryConfig
  { getHistory    :: IO [T.Text]
  , writeHistory  :: [T.Text] -> IO ()
  , appendToHistory :: T.Text -> [T.Text] -> [T.Text]
  , entryLimit    :: Int
  }

instance Def HistoryConfig where
  def = historyFile (withHomeDir ".local/state/fokshell/history") 10000

withHomeDir :: FilePath -> IO FilePath
withHomeDir p = getHomeDirectory <&> (</> p)

historyFile :: IO FilePath -> Int -> HistoryConfig
historyFile path limit = HistoryConfig {
    getHistory = do
    path' <- path
    doesFileExist path' >>= (`unless` do
      createDirectoryIfMissing True $ takeDirectory path'
      createFile path' (ownerReadMode B..|. ownerWriteMode) >>= closeFd
      )
    reverse . T.split (=='\n') <$> T.readFile path'
  , writeHistory = \history -> (path >>= (`T.writeFile` T.intercalate "\n" (reverse history)))
  , appendToHistory = \x' xs -> let x = T.strip x' in bool id (x:) (not (T.null x) && Just x /= head' xs) xs
  , entryLimit = limit
  }

data History = History
  {
    history     :: TVar [T.Text]
  , historyIndex:: TVar (Maybe (Int, T.Text))
  , historyConfig :: HistoryConfig
  }

instance Configurable History HistoryConfig where
  configure historyConfig = do
    atomically $ do
      history <- newTVar []
      historyIndex <- newTVar Nothing
      pure History {..}

instance Module' History ShellAction where
  initHook m = do
    history <- liftIO m.historyConfig.getHistory
    liftIO . atomically $ writeTVar m.history history
  exitHook m = do
    history <- liftIO $ readTVarIO m.history
    liftIO $ m.historyConfig.writeHistory (take m.historyConfig.entryLimit history)
  resetHook m = (liftIO . atomically $ writeTVar m.historyIndex Nothing) $> True
  preHook m (KeyModifiers 0, Enter) = do
    inputM <- get @Input
    input <- liftIO $ readTVarIO inputM.input
    liftIO . atomically $ do
      history <- readTVar m.history
      writeTVar m.history $ m.historyConfig.appendToHistory input history
      writeTVar m.historyIndex Nothing
    pure True
  preHook m (KeyModifiers 0, Arrow Up) = do
    inputM <- get @Input
    input <- liftIO $ readTVarIO inputM.input
    cursorLocation <- liftIO $ readTVarIO inputM.cursorLocation
    when (T.length input - cursorLocation > 0) (moveInputCursor inputM DLeft $ T.length input - cursorLocation)
    newInput <- liftIO . atomically $ do
      historyIndex <- readTVar m.historyIndex
      history <- readTVar m.history
      new <- case historyIndex of
          Nothing -> case history of 
            []      -> pure ""
            (x:_)  -> do
              writeTVar m.historyIndex $ Just (0, input)
              writeTVar inputM.input x
              pure x
          Just (i, r) -> let j = min (length history - 1) (i+1) in do
            writeTVar m.historyIndex $ Just (j, r)
            writeTVar inputM.input $ history !! j
            pure $ history !! j
      writeTVar inputM.cursorLocation 0
      pure new
    redrawFromCursor inputM $ Just $ T.length newInput
    liftIO $ moveCursor DRight (T.length newInput) >> hFlush stdout
    pure False
  preHook m (KeyModifiers 0, Arrow Down) = do
    inputM <- get @Input
    input <- liftIO $ readTVarIO inputM.input
    cursorLocation <- liftIO $ readTVarIO inputM.cursorLocation
    when (T.length input - cursorLocation > 0 ) (moveInputCursor inputM DLeft (T.length input - cursorLocation))
    newInput <- liftIO . atomically $ do
      historyIndex <- readTVar m.historyIndex
      history <- readTVar m.history
      new <- case historyIndex of
          Nothing -> pure ""
          Just (0,r) -> do
            writeTVar m.historyIndex Nothing
            writeTVar inputM.input r
            pure r
          Just (i, r) -> let j = max 0 (i-1) in do
            writeTVar m.historyIndex $ Just (j, r)
            writeTVar inputM.input $ history !! j
            pure $ history !! j
      writeTVar inputM.cursorLocation 0
      pure new
    redrawFromCursor inputM $ Just $ T.length $ newInput
    liftIO $ moveCursor DRight $ T.length $ newInput
    pure False
  preHook _m  _ = pure True
  postHook _m _ = pure True
