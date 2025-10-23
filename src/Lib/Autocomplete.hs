{-# LANGUAGE OverloadedStrings #-}
module Lib.Autocomplete where

import qualified Data.Text as T
import Lib.Primitive
import Lib.Format
import Lib.ColorScheme
import Data.Maybe (isJust)
import Data.Functor
import System.Directory (findExecutable)
import Control.Monad (when)

{- input -> history -> most related autocompletes -}
type AutocompleteModel = T.Text -> [T.Text] -> IO [T.Text]

defaultModel :: AutocompleteModel
defaultModel t = undefined

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
