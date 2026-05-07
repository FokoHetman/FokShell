{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Colorscheme where
import Lib.Config
import FokShell.Module
import qualified Data.Text as T
import Data.List (intercalate)
import Lib.Primitive

data ColorschemeModule = ColorschemeModule
  { colorschemes  :: [Colorscheme]
  , current       :: Int
  }

instance Module' ColorschemeModule ShellProcess where
  initHook' tc p = pure (tc,p)
  preHook' tc p e = pure (True,(tc,p))
  postHook' tc p e = pure (True,(tc,p))
  exitHook' tc p = pure (tc,p)


data Color = RGB Int Int Int

instance Show Color where
  show (RGB r g b) = intercalate ";" $ fmap show [r,g,b]

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
