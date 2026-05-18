{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}
module Main (main) where

import System.Posix (getEffectiveUserName)


import FokShell
import FokShell.Utils
import FokShell.Module
import FokShell.Module.Colorscheme
import FokShell.Module.Cursor
import FokShell.Module.DirectoryTracker
import FokShell.Module.History
import FokShell.Module.JobManager
import FokShell.Module.Parser
import FokShell.Module.Preprocessor
import FokShell.Module.Preprocessor.StringPreprocessors
import FokShell.Module.Prompt
import FokShell.Module.TabCompletion
import Lib.Primitive
import Lib.Format
import FokShell.Types
import Lib.Keys
import FokShell.Defaults

import System.Directory (getHomeDirectory)
import qualified Data.Text as T
import Control.Monad (when)
import Data.Functor
import Network.HostName
import Data.List (sort)


gruvbox :: Colorscheme
gruvbox = Colorscheme {
    userColors = [
      RGB 184 187 38
    , RGB 214 93 14
    ]
  , textColor = RGB 235 219 178
  , successColor = RGB 0 255 0
  , errorColor = RGB 255 0 0
  , infoColor = RGB 60 60 60
  }
gruvboxLight :: Colorscheme
gruvboxLight = Colorscheme {
    userColors = [
      RGB 235 219 178
    , RGB 214 93 14
    ]
  , textColor = RGB 129 132 133
  , successColor = RGB 0 255 0
  , errorColor = RGB 255 0 0
  , infoColor = RGB 29 32 33
  }

latte :: Colorscheme
latte = Colorscheme {
    userColors = [
      RGB 221 120 120
    , RGB 214 93 14
    ]
  , textColor = RGB 129 132 133
  , successColor = RGB 0 255 0
  , errorColor = RGB 255 0 0
  , infoColor = RGB 29 32 33
  }



colorSchemes :: [Colorscheme]
colorSchemes = [gruvbox, gruvboxLight, latte]

myPrompt :: Prompt
myPrompt = Prompt 
  { components =
    fmap (PromptComponent . TextComponent)
    [ (pure "[", \cs -> foreground $ cs.userColors!!0)
    , (T.pack <$> getEffectiveUserName, \cs -> bold <> foreground (cs.userColors!!0))
      , (pure "@", \cs -> foreground $ cs.userColors!!0)
      , (T.pack <$> getHostName, \cs -> bold <> foreground (cs.userColors!!0))
      , (pure ":", \cs -> foreground $ cs.userColors!!0)
      , (getFormattedDirectory, \cs -> bold <> foreground (cs.userColors!!0))
      , (pure "]$ ", \cs -> foreground $ cs.userColors!!0)
    ]
  }

myCoolPrompt :: Prompt
myCoolPrompt = Prompt
  { components = fmap (PromptComponent . TextComponent) [
      (pure "╭──", \cs -> foreground $ head cs.userColors)
    , (T.pack <$> getEffectiveUserName, \cs -> foreground (head cs.userColors) <> bold)
    , (pure "@", \cs -> foreground $ head cs.userColors)
    , (T.pack <$> getHostName, \cs -> foreground (head cs.userColors) <> bold)
    , (pure "───", \cs -> foreground $ head cs.userColors)
    , (getFormattedDirectory, \cs -> foreground (head cs.userColors) <> bold)
    , (pure "──────", \cs -> foreground $ head cs.userColors)
    , (pure "\n╰──>", \cs -> foreground $ head cs.userColors)
    ]
  }

myCursor :: Cursor
myCursor = Cursor
  {
    shape = BlinkingBar
  , color = RGB 255 255 255
  }
  

redraw :: ShellProcess -> IO ()
redraw proc@ShellProcess{shellConfig = c} = clear >> rPrompt >> dinput >> updCursor
  where
    clear = putStrf "\ESC[2K\r"
    rPrompt = displayPrompt' proc
    dinput = putStrf $ input c
    updCursor = when (cursorLoc c > 0) $ moveCursor DLeft $ cursorLoc c

main :: IO ()
main = fokshell $ def
    { binds = def ++ [
      {-((control, Character "t"), \proc -> 
        let config = shellConfig proc in let conf = config {colorScheme = nextColorScheme (colorScheme config)} in redraw conf $> proc {shellConfig = conf})-}
      ]
    , modules =
      [ Module (def :: DirectoryTracker)
      , Module myCursor
      , Module ColorschemeModule 
        { colorschemes = colorSchemes
        , current = 0
        }
      , Module (def :: TabCompletion)
        { maxSuggestions = 10
        , shadowText = True
        , sortAlgorithm = const sort
        }
      , Module (def :: History)
      , Module JobManager 
        { jobs = []
        }
      , Module (def :: ParserModule)
      , Module myPrompt
      ]
    }
