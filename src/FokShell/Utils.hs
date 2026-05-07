{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module FokShell.Utils where

import qualified Data.Text as T

import Control.Monad (when, filterM)
import System.Directory (getCurrentDirectory, getDirectoryContents, getPermissions, Permissions (executable), doesFileExist, canonicalizePath)
import System.Posix (getEnv)
import System.FilePath (takeFileName)
import Data.Dynamic (toDyn)
import Data.Functor
import Lib.Primitive
import Lib.Format
import Lib.Keys
import Lib.Config

updatePath :: ShellProcess -> IO ShellProcess 
updatePath proc = do
  path <- getEnv "PATH" >>= \case
    Just x -> pure x
    _ -> pure ""
  localFiles <- getDirectoryContents =<< getCurrentDirectory
  localExecutables <- fmap ("./" <>) <$> (mapM canonicalizePath localFiles >>= filterM (fmap executable . getPermissions))
  pathExecs <- (pathExecutables >>= mapM canonicalizePath . concat)  <&> fmap takeFileName
  let envcache = Entry ("executables", Cache [Entry ("PATH", toDyn path), Entry ("execs", toDyn $ fmap T.pack $ pathExecs ++ localExecutables)])

  pure proc {shellCache = Cache $ getCache (removeFromCache (shellCache proc) "executables") ++ [envcache]}
  
  where
    pathExecutables = mapM executablesInDir =<< getDirsInPath

replaceShortcuts :: [(T.Text, IO T.Text)] -> T.Text -> IO T.Text
replaceShortcuts (x:xs) t = snd x >>= \y -> replaceShortcuts xs (T.replace (fst x) y t)
replaceShortcuts [] text = pure text

clearLines :: Direction -> Int -> IO ()
clearLines _ 0 = putStr "\ESC[2K\r"
clearLines d i = putStr "\ESC[2K\r" >> moveCursor d 1 >> clearLines d (i-1)

redrawFromCursor :: ShellConfig -> IO ()
redrawFromCursor c = putStrf $ T.concat [erase, lefts, cursorCode]
  where
    erase = T.pack "\ESC[0K"
    lefts = T.reverse $ T.take (cursorLoc c) (T.reverse $ input c)
    cursorCode = if T.length lefts > 0 then T.concat ["\ESC[", T.pack $ show $ T.length lefts, "D"] else T.empty

moveCursor':: ShellConfig -> Direction -> Int -> IO ()
moveCursor' c DLeft  i = when (T.length (input c) > cursorLoc c) (moveCursor DLeft i)
moveCursor' c DRight i = when (cursorLoc c > 0)  (moveCursor DRight i)
moveCursor' _ _ _ = error "unsupported '-wrapped direction"


updateWithKey :: KeyEvent -> ShellProcess -> ShellProcess
updateWithKey event proc = proc {shellConfig = (shellConfig proc) {lastEvent=event}}
