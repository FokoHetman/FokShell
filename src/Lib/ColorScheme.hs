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
  -- id for a messy Eq
    scheme_id   :: T.Text
  -- user-friendly colors
  , colors      :: [(T.Text, Color)]
  -- system colors
  , textColor   :: Color
  , shadowText  :: Color
  , successColor:: Color
  , warningColor:: Color
  , errorColor  :: Color
}

instance Eq ColorScheme where
  (==) a b = scheme_id a == scheme_id b


generateColorShortcuts :: ColorScheme -> [(T.Text, IO T.Text)]
generateColorShortcuts c = [
    ("%t", wrap $ textColor c)
  , ("%s", wrap $ shadowText c)
  , ("%e", wrap $ errorColor c)
  , ("%w", wrap $ warningColor c)
  ] ++ fmap (bimap ("%"<>) wrap) (colors c)
  where
    wrap = (<&> asciiColor)

asciiColor :: RGB -> T.Text
asciiColor c = T.concat ["\ESC[38;2;", T.pack $ show c, "m"]

instance Def ColorScheme where
  def = ColorScheme {
    colors = []
  , textColor  = pure $ RGB 255 255 255
  , shadowText = pure $ RGB 80 80 80
  , errorColor = pure $ RGB 200 20 5
  , warningColor=pure $ RGB 252 225 42
  , successColor=pure $ RGB 50 168 82
  }
