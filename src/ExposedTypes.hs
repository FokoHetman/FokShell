{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module ExposedTypes where

import qualified Data.Text as T
import qualified Data.Bits as B


import System.Exit (exitSuccess)
import Control.Monad (when)
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Posix (getEffectiveUserName)

import Data.Functor
import System.IO (hFlush, stdout)

import Network.HostName

class Def a where
  def :: a


-- DISPLAY FUNCTIONS
eputStrf :: IO T.Text -> IO ()
eputStrf t = t >>= \x -> putStr (T.unpack x) <> hFlush stdout

putStrf :: T.Text -> IO ()
putStrf t = putStr (T.unpack t) <> hFlush stdout






data Direction = Up | Down | DRight | DLeft
    deriving (Show,Eq)
data KeyCode = Fn | Escape | Arrow Direction | Enter | Tab | Backspace | Delete | Character T.Text
    deriving (Show,Eq)
newtype KeyModifiers = KeyModifiers Int
    deriving (Show,Eq)
control :: KeyModifiers
control = KeyModifiers 1  -- 2^0

shift :: KeyModifiers
shift = KeyModifiers 2    -- 2^1

alt :: KeyModifiers
alt = KeyModifiers 4      -- 2^2

(.|.) :: KeyModifiers -> KeyModifiers -> KeyModifiers
(KeyModifiers a) .|. (KeyModifiers b) = KeyModifiers (a B..|. b)

type KeyEvent = (KeyModifiers, KeyCode)



type Hook = ShellProcess -> IO Bool -- bool tells whether to continue afterhand action

type Action = ShellProcess -> IO ShellProcess


data ShellHooks = ShellHooks 
  { haltHook  :: Hook -- things to do before a HALT (^C)
  , exitHook  :: Hook -- things to do before exitting (^D, exit, etc)
  , startHook :: Hook -- like rc
  , clearHook :: Hook
  }


defaultHaltHook :: Hook 
defaultHaltHook _ = do
  putStrLn "^C"
  pure True

defaultExitHook :: Hook
defaultExitHook _ = do
  putStrLn "\nexit"
  pure True

defaultClearHook :: Hook
defaultClearHook (ShellProcess conf _) = do
  if lastEvent conf == trigger conf then
    pure False
  else
    pure True

defaultStartHook :: Hook
defaultStartHook _ = pure True

instance Def ShellHooks where
  def = ShellHooks
    { haltHook  = defaultHaltHook
    , exitHook  = defaultExitHook
    , startHook = defaultStartHook
    , clearHook = defaultClearHook
    }

type PromptText = T.Text
data Swallow = Never | Swallowed PromptText
data Prompt  = SingleLine PromptText Swallow | MultiLine PromptText Swallow


getFormattedDirectory :: IO T.Text
getFormattedDirectory = do
  dir <- getCurrentDirectory
  home <- getHomeDirectory
  pure $ T.replace (T.pack home) "~" (T.pack dir)


data ShellConfig = ShellConfig
  { hooks     :: ShellHooks
  , prompt    :: Prompt
  
  , cursorLoc :: Int                  -- from the right, surprisingly
--, cursor    :: CursorConfig
  , input     :: T.Text
  , binds     :: [(KeyEvent, Action)]
  , lastEvent :: KeyEvent
  , trigger   :: KeyEvent             -- this should never be overriden globally, locally it should be overwritten with the keyevent trigger (example at ^L handling)
  }

data State = InputOutput

data ShellProcess = ShellProcess ShellConfig State


instance Def ShellConfig where
  def = ShellConfig
    { hooks = def
    , prompt = SingleLine "%d > " Never
    , input = ""
    
    , cursorLoc = 0

    , binds = def
    , lastEvent = (KeyModifiers 0, Escape)
    , trigger = (KeyModifiers 0, Escape)
    }

shortcuts :: [(T.Text, IO T.Text)]
shortcuts = [("%u", T.pack <$> getEffectiveUserName), ("%d", getFormattedDirectory), ("%h", T.pack <$> getHostName)]

replaceShortcuts :: [(T.Text, IO T.Text)] -> IO T.Text -> IO T.Text
replaceShortcuts (x:xs) text = do
  t <- text
  replaceShortcuts xs ((\y -> T.replace (fst x) y t) <$> snd x)
replaceShortcuts [] text = text

displayPrompt :: Prompt -> IO ()
displayPrompt = \case 
  SingleLine text _ -> eputStrf <$> formatted $ pure text
  MultiLine text _  -> eputStrf <$> formatted $ pure text
  where
    formatted :: IO T.Text -> IO T.Text
    formatted = replaceShortcuts shortcuts

redrawFromCursor :: ShellConfig -> IO ()
redrawFromCursor c = putStrf $ T.concat [erase, lefts, cursorCode]
  where
    erase = T.pack "\ESC[0K"
    lefts = T.reverse $ T.take (cursorLoc c) (T.reverse $ input c)
    cursorCode = if T.length lefts > 0 then T.concat ["\ESC[", T.pack $ show $ T.length lefts, "D"] else T.empty


haltAction :: Action
haltAction (ShellProcess config state) = displayPrompt (prompt config) $> ShellProcess (config {input = ""}) state

exitAction :: Action
exitAction (ShellProcess _ _) = exitSuccess

clearAction :: Action
clearAction (ShellProcess c s) = putStrLn "\ESC[2J\ESC[H" *> displayPrompt (prompt c) $> ShellProcess c s


instance Def [(KeyEvent, Action)] where
  def = [
        ((control, Character "c"), \(ShellProcess config state) -> haltHook (hooks config) (ShellProcess config state) >>= \x -> if x then haltAction (ShellProcess config state) else pure $ ShellProcess config state)
      , ((control, Character "d"), \(ShellProcess config state) -> exitHook (hooks config) (ShellProcess config state) >>= \x -> if x then exitAction (ShellProcess config state) else pure $ ShellProcess config state)
      , ((control, Character "l"), \(ShellProcess config state) -> clearHook(hooks config) (ShellProcess config {trigger=(control, Character "l")} state) >>= \x -> if x then clearAction (ShellProcess config state) else pure $ ShellProcess config state)
    ]


data CursorDirection = CLeft | CRight

moveCursor :: CursorDirection -> Int -> IO ()
moveCursor _ 0 = pure ()
moveCursor CLeft i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "D"
moveCursor CRight i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "C"

moveCursor':: ShellConfig -> CursorDirection -> Int -> IO ()
moveCursor' c CLeft  i = when (T.length (input c) > cursorLoc c) (moveCursor CLeft i)
moveCursor' c CRight i = when (cursorLoc c > 0)  (moveCursor CRight i)


updateWithKey :: KeyEvent -> ShellProcess -> ShellProcess
updateWithKey event (ShellProcess conf state) = ShellProcess conf {lastEvent=event} state


