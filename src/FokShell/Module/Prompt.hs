{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Prompt where
import FokShell.Module
import FokShell.Module.Colorscheme
import FokShell.Types
import FokShell.Utils
import Lib.Primitive
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.HostName
import Lib.Format (getFormattedDirectory)
import System.Posix (getEffectiveUserName)
import System.IO (stdout, hFlush)
import Control.Concurrent.STM (readTVarIO, TVar)
import Data.Functor
import Control.Monad.IO.Class (MonadIO(liftIO))

data PromptConfig = PromptConfig
  { prompt :: PromptComponent
  }

userComponent, hostComponent, dirComponent :: PromptComponent
userComponent = p . t $ T.pack <$> liftIO getEffectiveUserName
hostComponent = p . t $ T.pack <$> liftIO getHostName
dirComponent  = p . t $ liftIO getFormattedDirectory

instance Def PromptConfig where
  def = PromptConfig 
      { prompt = p $ GroupComponent
        [ p $ ColorschemeComponent $ pure . foreground . (.textColor)
        , p $ t2 bold
        , p $ t2 "["
        , p $ GroupComponent
          [ userComponent
          , p $ t2 "@"
          , hostComponent
          , p $ t2 ":"
          , dirComponent]
        , p $ t2 "]"
        , p $ t2 clear
        ]
        {-fmap (PromptComponent . TextComponent)
          [ (pure "[", foreground . (.textColor))
          , (T.pack <$> getEffectiveUserName, \cs -> bold <> foreground cs.textColor)
          , (pure "@", foreground . (.textColor))
          , (T.pack <$> getHostName, \cs -> bold <> foreground cs.textColor)
          , (pure ":", foreground . (.textColor))
          , (getFormattedDirectory, \cs -> bold <> foreground cs.textColor)
          , (pure "]$ ", foreground . (.textColor))
          ]
          -}
      }

data Prompt = Prompt
  { promptComponent :: TVar PromptComponent
  }

instance Module' Prompt ShellAction where
  initHook' tc =  do
    displayPrompt tc
  exitHook' _ = pure ()
  resetHook' tc = do
    liftIO $ putStr "\r\ESC[0J"
    displayPrompt tc
    pure True
  preHook' _ _ = pure True
  -- unluckily, the following conflicts with a lot of stuff :(
  --postHook' _ _ (KeyModifiers 0, Enter) = displayPrompt tc p $> (True,())
  postHook' _ _ = pure True

data PromptComponent where
  PromptComponent :: (PromptComponent' c) => c -> PromptComponent

p :: (PromptComponent' a) => a -> PromptComponent
p = PromptComponent

class PromptComponent' c where
  render' :: c -> ShellAction T.Text

render :: PromptComponent -> ShellAction T.Text
render (PromptComponent c) = render' c


displayPrompt :: Prompt -> ShellAction ()
displayPrompt (Prompt {promptComponent}) = do
  component' <- liftIO $ readTVarIO promptComponent
  render component' >>= liftIO . T.putStr >> liftIO (hFlush stdout)


displayPrompt' :: ShellAction ()
displayPrompt' = displayPrompt =<< get @Prompt


data TextComponent = TextComponent (ShellAction T.Text)
t :: ShellAction T.Text -> TextComponent
t = TextComponent
t2 :: T.Text -> TextComponent
t2 = TextComponent . pure
instance PromptComponent' TextComponent where
  render' (TextComponent txt) = txt


data ColorComponent = ColorComponent (Color -> T.Text) Color

instance PromptComponent' ColorComponent where
  render' (ColorComponent f c) = pure $ f c


data ColorschemeComponent = ColorschemeComponent (Colorscheme -> ShellAction T.Text)

instance PromptComponent' ColorschemeComponent where
  render' (ColorschemeComponent f) = do
    colorschemeModule <- getMaybe @ColorschemeModule
    colorscheme <- case colorschemeModule of
      Just m -> liftIO $ readTVarIO m.colorscheme
      Nothing-> pure def
    f colorscheme

data GroupComponent = GroupComponent [PromptComponent]

instance PromptComponent' GroupComponent where
  render' (GroupComponent []) = pure clear
  render' (GroupComponent (x:xs)) = do
    (<>) <$> render x <*> render' (GroupComponent xs)
    

bold,clear :: T.Text
bold = "\ESC[1m"
clear = "\ESC[0m"
