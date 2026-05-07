{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Prompt where
import FokShell.Module
import FokShell.Module.Colorscheme
import Lib.Config
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
data PromptModule = PromptModule
  { components :: [PromptComponent]
  }


instance Module' PromptModule ShellProcess where
  initHook' tc p = displayPrompt tc p $> (tc,p)
  preHook' tc p _ = pure (True,(tc,p))
  -- unluckily, the following conflicts with a lot of stuff :(
  --postHook' tc p (KeyModifiers 0, Enter) = displayPrompt tc p $> (True,(tc,p))
  postHook' tc p _ = pure (True,(tc,p))
  exitHook' tc p = pure (tc,p)

instance Def PromptModule where
  def = PromptModule 
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


displayPrompt :: PromptModule -> ShellProcess -> IO ()
displayPrompt (PromptModule {components}) p = mapM (render p) components >>= T.putStr . T.concat >> hFlush stdout


displayPrompt' :: ShellProcess -> IO ()
displayPrompt' proc = mapM_ (`displayPrompt` proc) $ requestModule (Proxy @PromptModule) proc.shellConfig.modules


data TextComponent = TextComponent (IO T.Text, Colorscheme -> T.Text)
instance PromptComponent' TextComponent where
  render' proc (TextComponent (t, formatting)) = (<>clear) . (formatting cscheme <>) <$> t
    where
      cscheme = case requestModule (Proxy @ColorschemeModule) proc.shellConfig.modules of
        [] -> def
        (x:_) -> x.colorschemes !! x.current

bold = "\ESC[1m"
clear = "\ESC[0m"
