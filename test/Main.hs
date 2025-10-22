{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Posix (getEffectiveUserName)


import FokShell
import ExposedTypes
import Lib.ColorScheme
import Lib.Primitive
import Data.Text as T

import Network.HostName

myHooks :: ShellHooks
myHooks = def

myColorScheme :: ColorScheme
myColorScheme = def {
    colors = [
      ("c0", pure $ RGB 184 187 38)
    , ("c1", pure $ RGB 214 93 14)
    ]
  , textColor = pure $ RGB 235 219 178
  , shadowText = pure $ RGB 29 32 33
  }

myPrompt :: Prompt
myPrompt = SingleLine (r "%c0%B[%u@%h:%d]$ %cl") $ Swallowed (r "%c0%B%d> %cl")
  where
    r = replaceShortcuts $ [("%B", pure "\ESC[1m"), ("%cl", pure "\ESC[0m"), ("%u", T.pack <$> getEffectiveUserName), ("%d", getFormattedDirectory), ("%h", T.pack <$> getHostName)] ++ generateColorShortcuts myColorScheme

{-
 - cool example for docs
 - MultiLine 
  (mapM (replaceShortcuts [("%u", T.pack <$> getEffectiveUserName), ("%d", getFormattedDirectory), ("%h", T.pack <$> getHostName)]) [
    "╭──%u@%h───%d──────",
    "╰──>"
  ]) $ Swallowed (replaceShortcuts [("%u", T.pack <$> getEffectiveUserName), ("%d", getFormattedDirectory), ("%h", T.pack <$> getHostName)] "%u> ")
-}

main :: IO ()
main = do
  fokshell $ def
    { hooks = myHooks
    , prompt = myPrompt
    , colorScheme = myColorScheme
    , binds = def -- ++ [
      --((control, Character "f"), \(ShellProcess config state) -> )
    --]
    }
