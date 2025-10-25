{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Posix (getEffectiveUserName)


import FokShell
import ExposedTypes
import Lib.ColorScheme
import Lib.Primitive
import Lib.Format

import qualified Data.Text as T

import Control.Monad (when)
import Data.Functor

import Network.HostName

myHooks :: ShellHooks
myHooks = def

myColorScheme :: ColorScheme
myColorScheme = def {
    scheme_id = "gruvbox"
  , colors = [
      ("c0", pure $ RGB 184 187 38)
    , ("c1", pure $ RGB 214 93 14)
    ]
  , textColor = pure $ RGB 235 219 178
  , shadowText = pure $ RGB 29 32 33
  }

myLightColorScheme :: ColorScheme
myLightColorScheme = def {
    scheme_id = "light"
  , colors = [
      ("c0", pure $ RGB 235 219 178)
    , ("c1", pure $ RGB 214 93 14)
    ]
  , textColor = pure $ RGB 29 32 33
  , shadowText = pure $ RGB 29 32 33
  }

colorSchemes :: [ColorScheme]
colorSchemes = [myColorScheme, myLightColorScheme]

{- TODO: make this work. Probably make Prompt/prompt's text a ~ function that takes a colorscheme -} 
myPrompt :: ColorScheme -> Prompt
myPrompt cs = SingleLine (r "%c0%B[%u@%h:%d]$ %cl") $ Swallowed (r "%c0%B%d> %cl")
  where
    r = replaceShortcuts $ [("%B", pure "\ESC[1m"), ("%cl", pure "\ESC[0m"), ("%u", T.pack <$> getEffectiveUserName), ("%d", getFormattedDirectory), ("%h", T.pack <$> getHostName)] ++ generateColorShortcuts cs

{-
 - cool example for docs
 - MultiLine 
  (mapM (replaceShortcuts [("%u", T.pack <$> getEffectiveUserName), ("%d", getFormattedDirectory), ("%h", T.pack <$> getHostName)]) [
    "╭──%u@%h───%d──────",
    "╰──>"
  ]) $ Swallowed (replaceShortcuts [("%u", T.pack <$> getEffectiveUserName), ("%d", getFormattedDirectory), ("%h", T.pack <$> getHostName)] "%u> ")
-}

redraw :: ShellConfig -> IO ()

redraw c = clear >> rPrompt >> dinput >> updCursor
  where
    clear = putStrf "\ESC[2K\r"
    rPrompt = displayPrompt (prompt c $ colorScheme c)
    dinput = putStrf $ input c
    updCursor = when (cursorLoc c > 0) $ moveCursor DLeft $ cursorLoc c

main :: IO ()
main = do
  fokshell $ def
    { hooks = myHooks
    , prompt = myPrompt
    , colorScheme = myColorScheme
    , binds = def ++ [
      ((control, Character "t"), \(ShellProcess config state) -> 
        let conf = config {colorScheme = nextColorScheme (colorScheme config)} in redraw conf $> ShellProcess conf state)
    ]
    }
    where
      index cur = indexOf cur colorSchemes + 1
      nextColorScheme current = colorSchemes!!(if index current>=length colorSchemes then 0 else index current)

indexOf x xs = indexOf' x xs 0
indexOf' x (y:ys) i = if x==y then i else indexOf' x ys (i+1)
indexOf' x [] i = -1
