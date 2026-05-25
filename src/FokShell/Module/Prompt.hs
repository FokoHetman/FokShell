{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Prompt where
import FokShell.Module
import FokShell.Module.Colorscheme
import FokShell.Types
import Lib.Primitive
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.HostName
import Lib.Format (getFormattedDirectory)
import System.Posix (getEffectiveUserName)
import System.IO (stdout, hFlush)
import Data.Proxy
import Control.Concurrent.STM (readTVarIO)
import Data.Functor
data Prompt = Prompt
  { components :: [PromptComponent]
  }


instance Module' Prompt ShellConfig where
  initHook' tc conf =  do
    tc' <- (readTVarIO tc)
    conf' <- (readTVarIO conf)
    displayPrompt tc' conf'
  exitHook' _ _ = pure ()
  resetHook' _ _ = pure ()
  preHook' _ _ _ = pure True
  -- unluckily, the following conflicts with a lot of stuff :(
  --postHook' _ _ (KeyModifiers 0, Enter) = displayPrompt tc p $> (True,())
  postHook' _ _ _ = pure True

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
  render' :: ShellConfig -> c -> IO T.Text

render :: ShellConfig -> PromptComponent -> IO T.Text
render p (PromptComponent c) = render' p c


displayPrompt :: Prompt -> ShellConfig -> IO ()
displayPrompt (Prompt {components}) p = mapM (render p) components >>= T.putStr . T.concat >> hFlush stdout


displayPrompt' :: ShellConfig -> IO ()
displayPrompt' proc = mapM readTVarIO (requestModule @Prompt proc.modules) >>= mapM_ (`displayPrompt` proc)


data TextComponent = TextComponent (IO T.Text, Colorscheme -> T.Text)
instance PromptComponent' TextComponent where
  render' proc (TextComponent (t, formatting)) = do
    cscheme <- case requestModule @ColorschemeModule proc.modules of
          [] -> pure def
          (x':_) -> readTVarIO x' <&> \x -> x.colorschemes !! x.current
    (<>clear) . (formatting cscheme <>) <$> t

bold,clear :: T.Text
bold = "\ESC[1m"
clear = "\ESC[0m"
