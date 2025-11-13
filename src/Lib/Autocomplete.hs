{-# LANGUAGE OverloadedStrings #-}
module Lib.Autocomplete where

import qualified Data.Text as T
import Lib.Primitive
import Lib.Format
import Lib.ColorScheme
import Data.Maybe (isJust, fromMaybe, isNothing)
import Data.Functor
import System.Directory (findExecutable, getCurrentDirectory, getDirectoryContents, getPermissions, Permissions (executable), doesDirectoryExist, canonicalizePath)
import Control.Monad (when, liftM, filterM, join, unless)
import System.Environment (getEnv)
import Debug.Trace (traceShow)

import System.FilePath.Posix ((</>))
import Data.Char (isSpace)
import Data.Bool (bool)
import Control.Arrow (Arrow(first))
import Data.Foldable (for_)

{- input -> cursor location -> history -> most related autocompletes in form of (CurrentWord, WholeQuery) -}
type AutocompleteModel = T.Text -> Int -> [T.Text] -> [T.Text] -> IO ([T.Text], [T.Text])

defaultModel :: AutocompleteModel
defaultModel inp cursor hist executables = if isMatchingExecutable then do
    let matches = filter (T.isPrefixOf fstWord) executables
    pure (matches, wholeMatches)
  else do 
    --print fstWord
    --print curWord
    pure ([], wholeMatches)
  where
    pathExecutables = mapM executablesInDir =<< getDirsInPath

    (matchIndex, cursorIndex) = extractIndexes inp cursor
    curWord = case matchIndex of
      Just x -> T.words inp !! x
      Nothing-> ""
    fstWord = fstWord' $ T.words inp
    fstWord' (x:__) = x
    fstWord' [] = ""
    isMatchingExecutable = matchIndex == Just (length (T.words inp)-1)

    wholeMatches = filter (T.isPrefixOf inp) hist


-- stop using fstWord == curWord you fucking piece of shit it makes the thing break when `command command`

extractIndexes text cursor = case f cursor (T.reverse text) of
      Nothing -> (Nothing, Nothing)
      Just (a,b) -> bool (Nothing, Nothing) (Just a, Just b) (a<length ( T.words text))
      where
        f :: Int -> T.Text -> Maybe (Int, Int)
        f cursor text
          | T.null text = Just (0, 0)
          | T.head text == ' ' = Nothing
          | and [T.length (head $ T.words text) > cursor, not . isSpace $ T.head text] = Just (0, cursor)
          | otherwise =  (T.stripPrefix (head $ T.words text) text) 
            >>= (\x -> (bool (f (cursor - T.length (head $ T.words text) - countLeadingWhitespace x) (T.strip x)) Nothing (countLeadingWhitespace x > cursor - T.length (head $ T.words text))))
            <&> first (1+)

-- when cursorIndex == matchedLength, redraw entire prompt

defaultHook :: T.Text -> Int -> ColorScheme -> ([T.Text], [T.Text]) -> IO ()
defaultHook t i c m = unless (T.null t) $
    bool
      ({-when (i >= T.length t - T.length fstWord) re-add I thibk-}resetCursor>> displayExecutable >> moveToCursor >> toEOW >> displayPrediction >> displayRight >> retrieveRight >> retrievePrediction >> fromEOW)
      ({-traceShow ("left: `" <> left <> "`:" <> "right: `" <> right <> "`:") $ -}resetCursor >> putStrf "\ESC[0K" >> displayExecutable >> displayLeft >> displayCurrent >> displayPrediction >> displayRight >> retrieveCursor >> retrievePrediction)
      (isNothing matchIndex)
  where
    fstWord = head $ T.words t

    {-toEOW = when (cursorIndex > 0) $ 
        moveCursor DRight cursorIndex
    fromEOW = when (cursorIndex > 0) $
        moveCursor DLeft cursorIndex-}
    (toEOW, fromEOW) = case cursorIndex of
      Just x -> (when (x > 0) $ moveCursor DRight x,
                 when (x>0) $ moveCursor DLeft x)
      Nothing -> (pure (), pure ())
    (matchIndex, cursorIndex) = extractIndexes t i

    curWord = matchIndex <&> (reverse (T.words t)!!) 

    displayCurrent = for_ curWord putStrf

    pred = case fst m of 
      (x:_) -> T.stripPrefix fstWord x
      [] -> Nothing
    --displayPrediction = putStrf pred
    (displayPrediction, retrievePrediction) = case pred of 
      Just p -> (shadowText c >>= putStrf . (<>p<>"\ESC[0m") . asciiColor, when (T.length p>0) $ moveCursor DLeft $ T.length p)
      Nothing -> (pure (), pure ())

    (curWordLeft, curWordRight) = case (curWord, cursorIndex) of
      (Just x, Just y) -> (T.take (T.length x - y) x, T.reverse $ T.take y $ T.reverse x)
      _ -> ("", "")
    displayRest = for_ (T.stripPrefix fstWord $ T.stripStart t) putStrf

    left = bool "" (unwrap $ T.stripPrefix fstWord $ T.take (T.length t - i - T.length curWordLeft) t) (T.length t > i)
    displayLeft = putStrf left

    -- todo: strip if fstWord is the prefix
    right = bool r (unwrap $ T.stripPrefix fstWord r) (fstWord `T.isPrefixOf` r)  where r = T.reverse $ T.take (i - T.length curWordRight) $ T.reverse t
    displayRight = putStrf right
    retrieveRight = when (T.length right>0) $ moveCursor DLeft $ T.length right
    rest = fromMaybe "" $ T.stripPrefix fstWord $ T.stripStart t
    
    resetCursor = when (T.length t > i) $ moveCursor DLeft (T.length t - i)

    retrieveCursor = when (r > 0) $ moveCursor DLeft r where r = i


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

-- horrible sin
unwrap (Just x) = x

countLeadingWhitespace :: T.Text -> Int
countLeadingWhitespace t
      | T.null t  = 0
      | not (isSpace $ T.head t) = 0
      | isSpace (T.head t) = countLeadingWhitespace (T.tail t) + 1
      | otherwise = 0
