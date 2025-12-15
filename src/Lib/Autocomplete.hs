{-# LANGUAGE OverloadedStrings #-}
module Lib.Autocomplete where

import qualified Data.Text as T
import Lib.Primitive
import Lib.Format
import Lib.ColorScheme
import Data.Maybe (isJust, fromMaybe, isNothing)
import Data.Functor
import System.Directory (findExecutable, getCurrentDirectory, getDirectoryContents, getPermissions, Permissions (executable, readable), doesDirectoryExist, canonicalizePath)
import Control.Monad (when, liftM, filterM, join, unless)
import System.Environment (getEnv)
import Debug.Trace (traceShow)

import System.FilePath.Posix ((</>), takeDirectory)
import Data.Char (isSpace)
import Data.Bool (bool)
import Control.Arrow (Arrow(first))
import Data.Foldable (for_)
import Data.List (isPrefixOf)

import Language.Parser

{- input -> cursor location -> history -> most related autocompletes in form of (CurrentWord, WholeQuery) -}
type AutocompleteModel = T.Text -> Int -> [T.Text] -> [T.Text] -> IO ([T.Text], [T.Text])
{- todo: make input a Record, consolidate last 2 args into passing Caches -}

defaultModel :: AutocompleteModel
defaultModel inp cursor hist executables = if isMatchingExecutable then do
    let matches = filter (T.isPrefixOf fstWord) executables
    pure (matches, wholeMatches)
  else do
    let d = takeDirectory $ T.unpack curWord
    --print "///"
    --print d
    --print "///"
    exists <- doesDirectoryExist d
    if exists then getPermissions d >>= \x ->
      if readable x then do
        localFiles <- getDirectoryContents d
        --print "\\\\\\"
        --print localFiles
        --print "\\\\\\"
        let matches = filter (T.isPrefixOf curWord) $ bool id (T.pack . (d</>) . T.unpack) (T.pack d `T.isPrefixOf` curWord) <$> fmap T.pack localFiles
        --print fstWord
        --print curWord
        pure (matches, wholeMatches)
      else
        pure ([], [])
    else
      pure ([], [])
  where
    pathExecutables = mapM executablesInDir =<< getDirsInPath

    (matchIndex, cursorIndex) = extractIndexes inp cursor
    curWord = maybe "" (reverse (T.words inp)!!) matchIndex
    fstWord = fstWord' $ T.words inp
    fstWord' (x:__) = x
    fstWord' [] = ""
    isMatchingExecutable = matchIndex == Just (length (T.words inp)-1)

    wholeMatches = filter (T.isPrefixOf inp) hist

-- theoritically that's not needed
{-languageModel :: AutocompleteModel
languageModel inp cursor hist executables = case parsed of
    Just ("", ProgramCall e a)  -> pure ([], []) -- good luck for god has long abandoned you.
    Nothing  -> pure ([], [])
  where
    parsed = runParser parseExpr inp
    (matchIndex, cursorIndex) = extractIndexes inp cursor-}
-- guides of the forever rotten:
-- make a recursive function calculating length of all words dependent on the node.
-- make whitespace a passed list probably idfk really
-- it's gonna work trust me brutha

-- once you figure that out a hook is gonna be easy trust be brotha



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


languageHook :: T.Text -> Int -> ColorScheme -> ([T.Text], [T.Text]) -> IO ()
languageHook input cursor cscheme model = unless (T.null input) $
  bool
    afterCursor -- redraw only right, as you're within a word so now quirky stuff
    whole       -- redraw everything, as you're jumping between words.
    (isNothing matchIndex)
  where
    (matchIndex, cursorIndex) = extractIndexes input cursor

    afterCursor, whole :: IO ()

    curWord = matchIndex <&> (reverse (T.words input)!!)

    whitespace = segmentWhiteSpace input
    whitespace' = case matchIndex of 
      Just x  -> reverse $ take x $ reverse $ segmentWhiteSpace input
      Nothing -> []
    whole = resetCursor input cursor >> eraseRight >> langAsAnsi input whitespace cscheme cursor >>= putStrf
    afterCursor = toCurWordStart >> eraseRight >> displayCurrent >> displayPrediction >> putStrf right >> retrieveCursor >> retrievePrediction

    -- TODO: add `langAsAnsi` contextual modes {Executable, Argument}

    displayCurrent = for_ curWord (\x -> langAsAnsi x whitespace' cscheme cursor >>= putStrf)

    pred = case fst model of 
      (x:_) -> curWord >>= (`T.stripPrefix` x)
      [] -> Nothing
    --displayPrediction = putStrf pred
    (displayPrediction, retrievePrediction) = case pred of 
      Just p -> (shadowText cscheme >>= putStrf . (<>p<>"\ESC[0m") . asciiColor, when (T.length p>0) $ moveCursor DLeft $ T.length p)
      Nothing -> (pure (), pure ())
    retrieveCursor = when (r > 0) $ moveCursor DLeft r where r = cursor

    right = T.reverse $ T.take (cursor - T.length curWordRight) $ T.reverse input
    (curWordLeft, curWordRight) = case (curWord, cursorIndex) of
      (Just x, Just y) -> (T.take (T.length x - y) x, T.reverse $ T.take y $ T.reverse x)
      _ -> ("", "")

    toCurWordStart = bool (pure ()) (moveCursor DLeft (T.length curWordLeft)) (T.length curWordLeft > 0)

resetCursor :: T.Text -> Int -> IO ()
resetCursor t i = when (T.length t > i) $ moveCursor DLeft (T.length t - i)

eraseRight :: IO ()
eraseRight = putStrf "\ESC[0K"

defaultHook :: T.Text -> Int -> ColorScheme -> ([T.Text], [T.Text]) -> IO ()
defaultHook t i c m = unless (T.null t) $
    bool
      ({-when (i >= T.length t - T.length fstWord) re-add I thibk-}resetCursor t i >> displayExecutable >> moveToCursor >> toEOW >> displayPrediction >> displayRight >> retrieveRight >> retrievePrediction >> fromEOW)
      (resetCursor t i>> eraseRight >> displayExecutable >> displayLeft >> displayCurrent >> displayPrediction >> displayRight >> retrieveCursor >> retrievePrediction)
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
      (x:_) -> curWord >>= (`T.stripPrefix` x)
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


segmentWhiteSpace :: T.Text -> [T.Text]
segmentWhiteSpace t
      | T.null t = []
      | isSpace (T.head t) = (T.pack [' ' | _<-[1..T.length t - T.length t2]]):segmentWhiteSpace t2 
      | otherwise = segmentWhiteSpace $ T.dropWhile (/=' ') t
      where t2 = T.dropWhile (==' ') t
