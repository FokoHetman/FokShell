module Main (main) where

import FokShell
import Data.Text as T

myHooks :: ShellHooks
myHooks = def

myPrompt :: Prompt
myPrompt = SingleLine (T.pack "[%u:%d] $ ") Never

main :: IO ()
main = do
  fokshell $ def
    { hooks = myHooks
    , prompt = myPrompt
    , binds = def
    }
