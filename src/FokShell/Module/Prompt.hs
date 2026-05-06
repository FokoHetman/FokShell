{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Prompt where
import FokShell.Module
import Lib.Config (ShellProcess)
import Lib.Keys
import Lib.Primitive
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Functor
import Network.HostName
import Lib.Format (getFormattedDirectory)
import System.Posix (getEffectiveUserName)
import System.IO (stdout, hFlush)
import Debug.Trace
data PromptModule = PromptModule
  { components :: [PromptComponent]
  }

instance Module' PromptModule ShellProcess where
  initHook' tc p = displayPrompt tc p $> (tc,p)
  preHook' tc p e = pure (True,(tc,p))
  postHook' tc p (KeyModifiers 0, Enter) = displayPrompt tc p $> (True,(tc,p))
  postHook' tc p _ = pure (True,(tc,p))
  exitHook' tc p = pure (tc,p)

instance Def PromptModule where
  def = PromptModule 
      { components =
          fmap (PromptComponent . TextComponent)
	  [ (pure "[", "")
          , (T.pack <$> getEffectiveUserName, bold)
          , (pure "@", "")
          , (T.pack <$> getHostName, bold)
          , (pure ":", "")
          , (getFormattedDirectory, bold)
          , (pure "]$ ", "")
	  ]
      }

data PromptComponent where
  PromptComponent :: (PromptComponent' c) => c -> PromptComponent

class PromptComponent' c where
  render' :: ShellProcess -> c -> IO T.Text

render :: ShellProcess -> PromptComponent -> IO T.Text
render p (PromptComponent c) = render' p c


displayPrompt :: PromptModule -> ShellProcess -> IO ()
displayPrompt (PromptModule {components}) p = mapM (render p) components >>= T.putStr . T.concat >> hFlush stdout


data TextComponent = TextComponent (IO T.Text, T.Text)
instance PromptComponent' TextComponent where
  render' _ (TextComponent (t, fmt)) = (<>clear) . (fmt <>) <$> t


bold = "\ESC[1m"
clear = "\ESC[0m"

-- TODO
-- move colorscheme here
-- make TextComponent's 2nd arg be the theme/formatting, ex:
-- bold <> primaryColor
