{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.History where

import qualified Data.Bits as B
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Functor ((<&>))
import System.FilePath ((</>), takeDirectory)

import Lib.Primitive
import System.Directory (getHomeDirectory, doesFileExist, createDirectory, createDirectoryIfMissing)
import FokShell.Module
import Lib.Config
import Lib.Keys
import Control.Monad (unless)
import System.Posix (createFile, ownerReadMode, ownerWriteMode, closeFd)

data HistoryModule = HistoryModule
  {
    history     :: [T.Text]
  , getHistory  :: IO [T.Text]
  , writeHistory:: [T.Text] -> IO ()
  , entryLimit  :: Int
  }

instance Def HistoryModule where
  def = historyFile (withHomeDir "") 10000

withHomeDir :: FilePath -> IO FilePath
withHomeDir p = getHomeDirectory <&> (</> p)


historyFile :: IO FilePath -> Int -> HistoryModule
historyFile path limit = HistoryModule {
    history = []
  , getHistory = do
    path' <- path
    doesFileExist path' >>= (`unless` do
      createDirectoryIfMissing True $ takeDirectory path'
      createFile path' (ownerReadMode B..|. ownerWriteMode) >>= closeFd
      )
    reverse . T.split (=='\n') <$> T.readFile path'
  , writeHistory = \history -> (path >>= (`T.writeFile` T.intercalate "\n" (reverse history)))
  , entryLimit = limit
  }

instance Module' HistoryModule ShellProcess where
  initHook' tc p = do
    history <- tc.getHistory
    pure (tc {history = history}, p)
  preHook' tc p (KeyModifiers 0, Enter) = pure (True,(tc {history = p.shellConfig.input:tc.history},p))
  preHook' tc p _ = pure (True,(tc,p))
  postHook' tc p = pure ((tc,p))
  exitHook' tc p = tc.writeHistory (take tc.entryLimit tc.history) >> pure (tc,p)
