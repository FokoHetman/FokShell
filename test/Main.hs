{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Posix (getEffectiveUserName)


import FokShell
import ExposedTypes
import Data.Text as T

import Network.HostName

myHooks :: ShellHooks
myHooks = def

myColorScheme :: ColorScheme
myColorScheme = def {
    color0 = pure $ RGB 152 151 26
  , color1 = pure $ RGB 214 93 14
  , textColor = pure $ RGB 251 241 199
  , shadowText = pure $ RGB 29 32 33
  }

myPrompt :: Prompt
myPrompt = SingleLine (r "%c0[%u@%h:%d]$ %t") $ Swallowed (r "%d> ")
  where
    r = replaceShortcuts $ [("%u", T.pack <$> getEffectiveUserName), ("%d", getFormattedDirectory), ("%h", T.pack <$> getHostName)] ++ generateColorShortcuts myColorScheme

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
