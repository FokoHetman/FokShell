{-# LANGUAGE OverloadedStrings #-}
module Lib.Autocomplete where

import qualified Data.Text as T
import Lib.Primitive
import Lib.Format
import Lib.ColorScheme
import Data.Maybe (isJust)
import Data.Functor
import System.Directory (findExecutable, getCurrentDirectory, getDirectoryContents, getPermissions, Permissions (executable), doesDirectoryExist)
import Control.Monad (when, liftM, filterM, join)
import System.Environment (getEnv)
import Debug.Trace (traceShow)

import System.FilePath.Posix ((</>))

getDirsInPath :: IO [FilePath]
getDirsInPath = filterM doesDirectoryExist . fmap T.unpack . T.split (==':') . T.pack =<< getEnv "PATH"

executablesInDir :: FilePath -> IO [FilePath]
executablesInDir t = getDirectoryContents t >>= filterM (fmap executable . getPermissions . (t</>))


{- input -> cursor location -> history -> most related autocompletes in form of (CurrentWord, WholeQuery) -}
type AutocompleteModel = T.Text -> Int -> [T.Text] -> IO ([T.Text], [T.Text])

defaultModel :: AutocompleteModel
defaultModel inp cursor hist = if isMatchingExecutable then do

    --print curWord
    --print fstWord

    --getDirsInPath >>= mapM getDirectoryContents  >>= print
    
    localFiles <- getDirectoryContents =<< getCurrentDirectory
    localExecutables <- fmap ("./" <>) <$> filterM (fmap executable . getPermissions) localFiles

    --print "local:"
    --print localExecutables

    pathExecs <- pathExecutables <&> concat
    --print "path:"
    --print pathExecs
    --print("over")

    let matches = filter (T.isPrefixOf inp) $ fmap T.pack $ localExecutables ++ pathExecs
    
    pure (matches, wholeMatches)
  else do 
    --print fstWord
    --print curWord
    pure ([], [])
  where
    pathExecutables = mapM executablesInDir =<< getDirsInPath
    fstWord = fstWord' $ T.words inp
    fstWord' (x:__) = x
    fstWord' [] = ""
    curWord = curWord' cursor $ reverse $ T.words inp
    curWord' i (x:xs) = if T.length x > i then x else curWord' (i-T.length x) xs
    curWord' i _ = ""
    isMatchingExecutable = curWord == fstWord

    wholeMatches = filter (T.isPrefixOf inp) hist


defaultHook :: T.Text -> Int -> ColorScheme -> IO ()
defaultHook t i c = moveCursor DLeft (len - i) >> ((findExecutable (T.unpack fstWord) >>= formattedWord . isJust) >>= putStrf) >> moveCursor DRight (len - i - T.length fstWord)
  where
    len = T.length t
    fstWord = fstWord' $ T.words t
    fstWord' (x:_) = x
    fstWord' [] = ""
    formattedWord e = (if e || T.null fstWord then successColor c else errorColor c) <&> (<>fstWord<>"\ESC[0m") . asciiColor
data AutocompleteConfig = AutocompleteConfig {
    model     :: AutocompleteModel
  , redrawHook:: T.Text -> Int -> ColorScheme -> IO ()
  }

instance Def AutocompleteConfig where
  def = AutocompleteConfig {
    model = defaultModel
  , redrawHook = defaultHook
  }
