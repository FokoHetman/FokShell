{-# LANGUAGE OverloadedStrings #-}
module Lib.Autocomplete where

import qualified Data.Text as T
import Lib.Primitive
import Lib.Format
import Lib.ColorScheme
import Data.Maybe (isJust)
import Data.Functor
import System.Directory (findExecutable, getCurrentDirectory, getDirectoryContents, getPermissions, Permissions (executable), doesDirectoryExist)
import Control.Monad (when, liftM, filterM, join, unless)
import System.Environment (getEnv)
import Debug.Trace (traceShow)

import System.FilePath.Posix ((</>))
import Data.Char (isSpace)
import Data.Bool (bool)

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
    pure ([], wholeMatches)
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


-- stop using fstWord == curWord you fucking piece of shit it makes the thing break when `command command`


defaultHook :: T.Text -> Int -> ColorScheme -> ([T.Text], [T.Text]) -> IO ()
defaultHook t i c m = unless (T.null t) $
    when (i >= T.length t - T.length fstWord) (resetCursor>> displayExecutable >> moveToCursor)
  where
    fstWord = head $ T.words t

    rest = bool "" " " (T.length f>0) <> f
      where
        f = T.concat $ tail $ T.words t
    resetCursor = when (T.length t > i) $ moveCursor DLeft (T.length t - i)

    formattedWord e = (if e || T.null fstWord then successColor c else errorColor c) <&> (<>fstWord<>"\ESC[0m") . asciiColor
    displayExecutable = (findExecutable (T.unpack fstWord) >>= formattedWord . isJust) >>= putStrf

    moveToCursor = {-traceShow rest $ -}bool (when (T.length rest > i) $ moveCursor DRight (T.length rest - i)) (moveCursor DLeft (i - T.length rest)) (i > T.length rest) 


data AutocompleteConfig = AutocompleteConfig {
    model     :: AutocompleteModel
  , redrawHook:: T.Text -> Int -> ColorScheme -> ([T.Text], [T.Text]) -> IO ()
  }

instance Def AutocompleteConfig where
  def = AutocompleteConfig {
    model = defaultModel
  , redrawHook = defaultHook
  }

unwrap (Just x) = x

countLeadingWhitespace :: T.Text -> Int
countLeadingWhitespace t
      | T.null t  = 0
      | not (isSpace $ T.head t) = 0
      | isSpace (T.head t) = countLeadingWhitespace (T.tail t) + 1
      | otherwise = 0
