{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Prompt where
import FokShell.Module
import FokShell.Module.Colorscheme
import FokShell.Types
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
import Data.Proxy
import FokShell.Module.Colorscheme (foreground, Colorscheme)
data Prompt = Prompt
  { components :: [PromptComponent]
  }


instance Module' Prompt ShellProcess where
  initHook' tc p = displayPrompt tc p $> (tc,p)
  exitHook' tc p = pure (tc,p)
  resetHook' tc p = pure (tc, p)
  preHook' tc p _ = pure (True,(tc,p))
  -- unluckily, the following conflicts with a lot of stuff :(
  --postHook' tc p (KeyModifiers 0, Enter) = displayPrompt tc p $> (True,(tc,p))
  postHook' tc p _ = pure (True,(tc,p))

instance Def Prompt where
  def = Prompt 
      { components =
          fmap (PromptComponent . TextComponent)
            [ (pure "[", foreground . (.textColor))
            , (T.pack <$> getEffectiveUserName, \cs -> bold <> foreground cs.textColor)
            , (pure "@", foreground . (.textColor))
            , (T.pack <$> getHostName, \cs -> bold <> foreground cs.textColor)
            , (pure ":", foreground . (.textColor))
            , (getFormattedDirectory, \cs -> bold <> foreground cs.textColor)
            , (pure "]$ ", foreground . (.textColor))
            ]
      }

data PromptComponent where
  PromptComponent :: (PromptComponent' c) => c -> PromptComponent

class PromptComponent' c where
  render' :: ShellProcess -> c -> IO T.Text

render :: ShellProcess -> PromptComponent -> IO T.Text
render p (PromptComponent c) = render' p c


displayPrompt :: Prompt -> ShellProcess -> IO ()
displayPrompt (Prompt {components}) p = mapM (render p) components >>= T.putStr . T.concat >> hFlush stdout


displayPrompt' :: ShellProcess -> IO ()
displayPrompt' proc = mapM_ (`displayPrompt` proc) $ requestModule (Proxy @Prompt) proc.shellConfig.modules


data TextComponent = TextComponent (IO T.Text, Colorscheme -> T.Text)
instance PromptComponent' TextComponent where
  render' proc (TextComponent (t, formatting)) = (<>clear) . (formatting cscheme <>) <$> t
    where
      cscheme = case requestModule (Proxy @ColorschemeModule) proc.shellConfig.modules of
        [] -> def
        (x:_) -> x.colorschemes !! x.current

bold = "\ESC[1m"
clear = "\ESC[0m"
