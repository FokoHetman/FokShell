{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Colorscheme where
import FokShell.Types
import FokShell.Module
import qualified Data.Text as T
import Data.List (intercalate)
import Lib.Primitive
import Data.Char (chr)
import Debug.Trace (traceShow)

data ColorschemeModule = ColorschemeModule
  { colorschemes  :: [Colorscheme]
  , current       :: Int
  }

instance Module' ColorschemeModule ShellProcess where
  initHook' tc p = pure (tc,p)
  exitHook' tc p = pure (tc,p)
  resetHook' tc p = pure (tc, p)
  preHook' tc p e = pure (True,(tc,p))
  postHook' tc p e = pure (True,(tc,p))


data Color = RGB Int Int Int | Hex Int

paddingRight n x = x <> take (n-length x) (repeat '0')
paddingLeft n x = take (n-length x) (repeat '0') <> x

getDivisor a b basis
  | a>b = getDivisor a (b*basis) basis
  | otherwise = b `div` basis
toBase i b
  | i >= d = paddingRight (n+1) $ fin l <> toBase rest b
  | otherwise = paddingRight n $ fin i
  where
    fin x
      | x==0 = ""
      | x<10 = show x
      | otherwise = [chr $ x+87]
    d = getDivisor i b b
    n :: Int
    n = floor $ logBase (fromIntegral b :: Double) $ (fromIntegral d :: Double)
    l = i `div` d
    rest = i `rem` d

toHex, toRGB :: Color -> Color
toHex (RGB r g b) = Hex $ r*256*256 + g*256 + b
toHex (Hex x) = Hex x
toRGB (RGB r g b) = RGB r g b
toRGB (Hex x) = RGB r g b
  where
    r = x `div` (256*256)
    r_rem = x `rem` (256*256)
    g = r_rem `div` 256
    b = r_rem `rem` 256

instance Show Color where
  show (RGB r g b) = intercalate ";" $ fmap show [r,g,b]
  show (Hex i) = "#" <> paddingLeft 6 (toBase i 16)

data Colorscheme = Colorscheme
  { successColor  :: Color
  , errorColor    :: Color
  , infoColor     :: Color
  , textColor     :: Color
  , userColors    :: [Color]
  }

instance Def Colorscheme where
  def = Colorscheme
    { successColor = RGB 52 219 102
    , errorColor = RGB 219 77 52
    , infoColor = RGB 91 91 91
    , textColor = RGB 255 255 255
    , userColors = []
    }

foreground,background :: Color -> T.Text
foreground c = "\ESC[38;2;" <> T.show c <> "m"
background c = "\ESC[48;2;" <> T.show c <> "m"
