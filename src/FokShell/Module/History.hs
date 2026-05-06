{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.History where

import qualified Data.Bits as B
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Functor ((<&>))
import Data.Bool
import System.FilePath ((</>), takeDirectory)

import Lib.Primitive
import System.Directory (getHomeDirectory, doesFileExist, createDirectory, createDirectoryIfMissing)
import FokShell.Module
import Lib.Config
import Lib.Keys
import Control.Monad (unless, when)
import System.Posix (createFile, ownerReadMode, ownerWriteMode, closeFd)
import Lib.Format

import Data.Functor (($>))
import FokShell.Types (moveCursor', redrawFromCursor)

data HistoryModule = HistoryModule
  {
    history     :: [T.Text]
  , historyIndex:: Maybe (Int, T.Text)
  , getHistory  :: IO [T.Text]
  , appendToHistory :: T.Text -> [T.Text] ->  [T.Text]
  , writeHistory:: [T.Text] -> IO ()
  , entryLimit  :: Int
  }

instance Def HistoryModule where
  def = historyFile (withHomeDir ".config/fokshell/history") 10000

withHomeDir :: FilePath -> IO FilePath
withHomeDir p = getHomeDirectory <&> (</> p)

head' [] = Nothing
head' (x:xs) = Just x

historyFile :: IO FilePath -> Int -> HistoryModule
historyFile path limit = HistoryModule {
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
  }

instance Module' HistoryModule ShellProcess where
  initHook' tc p = do
    history <- tc.getHistory
    pure (tc {history = history}, p)
  preHook' tc p (KeyModifiers 0, Enter) = pure (True,(tc {history=tc.appendToHistory p.shellConfig.input tc.history, historyIndex = Nothing},p))
  preHook' tc p (KeyModifiers 0, Arrow Up) = when (T.length (input conf) - cursorLoc conf > 0 ) (moveCursor' conf DLeft (T.length (input conf) - cursorLoc conf)) >> 
            (\(tc', conf') ->  redrawFromCursor conf' {cursorLoc = T.length $ conf'.input} >>  moveCursor' conf' {cursorLoc = T.length $ conf'.input} DRight (T.length $ conf'.input) $> (True, (tc',p {shellConfig = conf' {cursorLoc = 0}})))
              (case tc.historyIndex of
              Nothing -> case tc.history of 
                []      -> (tc, conf)
                (x:_)  -> (tc {historyIndex = Just (0, input conf)}, conf {input = x})
              Just (i, r) -> let j = min (length tc.history - 1) (i+1) in (tc {historyIndex = Just (j, r)}, conf {input = tc.history!!j})
              )
          where
            conf = p.shellConfig
  preHook' tc p (KeyModifiers 0, Arrow Down) = when (T.length (input conf) - cursorLoc conf > 0 ) (moveCursor' conf DLeft (T.length (input conf) - cursorLoc conf)) >> 
            (\(tc', conf') ->  redrawFromCursor conf' {cursorLoc = T.length $ conf'.input} >>  moveCursor' conf' {cursorLoc = T.length $ conf'.input} DRight (T.length $ conf'.input) $> (True, (tc',p {shellConfig = conf' {cursorLoc = 0}})))
        (case tc.historyIndex of
              Nothing -> (tc, conf)
              Just (0,r) -> (tc {historyIndex=Nothing},conf {input=r})
              Just (i, r) -> let j = max 0 (i-1) in (tc {historyIndex = Just (j, r)}, conf {input = tc.history!!j})
              )
          where
            conf = p.shellConfig
  preHook' tc p _ = pure (True,(tc,p))
  postHook' tc p e = pure (True, (tc,p))
  exitHook' tc p = tc.writeHistory (take tc.entryLimit tc.history) >> pure (tc,p)
