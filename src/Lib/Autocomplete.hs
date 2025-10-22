module Lib.Autocomplete where

import qualified Data.Text as T
import Lib.Primitive

{- input -> history -> most related autocompletes -}
type AutocompleteModel = T.Text -> [T.Text] -> IO [T.Text]

defaultModel :: AutocompleteModel
defaultModel t = undefined


data AutocompleteConfig = AutocompleteConfig {
    model     :: AutocompleteModel
  , redrawHook:: T.Text -> Int -> IO ()
  }

instance Def AutocompleteConfig where
  def = AutocompleteConfig {
    model = defaultModel
  }
