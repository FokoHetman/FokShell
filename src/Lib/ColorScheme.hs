{-# LANGUAGE OverloadedStrings #-}
module Lib.ColorScheme where

import Data.Text as T
import Data.Functor

import Lib.Primitive
import Data.Bifunctor (Bifunctor(bimap))

data RGB = RGB Int Int Int
type Color = IO RGB

instance Show RGB where
  show (RGB r g b) = show r ++ ";" ++ show g ++ ";" ++ show b

data ColorScheme = ColorScheme {
  
  -- user-friendly colors
    colors      :: [(T.Text, Color)]
  , textColor   :: Color
  , shadowText  :: Color
  , errorColor  :: Color
  , warningColor:: Color
  -- I don't know what other colors are there
}

generateColorShortcuts :: ColorScheme -> [(T.Text, IO T.Text)]
generateColorShortcuts c = [
    ("%t", wrap $ textColor c)
  , ("%s", wrap $ shadowText c)
  , ("%e", wrap $ errorColor c)
  , ("%w", wrap $ warningColor c)
  ] ++ fmap (bimap ("%"<>) wrap) (colors c)
  where
    wrap = (<&> \x -> T.concat ["\ESC[38;2;", T.pack $ show x, "m"])

instance Def ColorScheme where
  def = ColorScheme {
    colors = []
  , textColor  = pure $ RGB 255 255 255
  , shadowText = pure $ RGB 80 80 80
  , errorColor = pure $ RGB 200 20 5
  , warningColor=pure $ RGB 252 225 42
  }
