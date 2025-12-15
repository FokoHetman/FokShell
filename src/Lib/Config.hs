{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Lib.Config where

import Lib.Autocomplete
import Lib.ColorScheme
import Lib.Primitive
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Bits as B
import Data.Dynamic (Dynamic)
import Lib.Keys
import Lib.Format
import System.Process

import System.Posix (fileExist, createFile, ownerWriteMode, ownerReadMode, closeFd)
import System.Directory (getHomeDirectory)

import System.FilePath ((</>))

import Debug.Trace

import Data.Functor

import GHC.IO.Handle
import Language.Parser
import GHC.IO.Exception (ExitCode)
import Control.Monad
import System.Exit (exitSuccess)


data Job = Job {
    pid     :: Maybe Pid
  , task    :: Task
  , stdoutj :: Maybe Handle
  , stderrj :: Maybe Handle
  , stdinj  :: Maybe Handle
  , last_ec :: ExitCode
  , pipeOut :: PipeType
  , pipeIn  :: PipeType
  , pipeErr :: PipeType
}

newtype JobMgr = JobMgr [Job]

{-
ESC[0 q 	changes cursor shape to steady block
ESC[1 q 	changes cursor shape to steady block also
ESC[2 q 	changes cursor shape to blinking block
ESC[3 q 	changes cursor shape to steady underline
ESC[4 q 	changes cursor shape to blinking underline
ESC[5 q 	changes cursor shape to steady bar
ESC[6 q 	changes cursor shape to blinking bar
-}

data CursorShape = SteadyBlock | BlinkingBlock | SteadyUnderline | BlinkingUnderline | SteadyBar | BlinkingBar

instance Show CursorShape where
  show BlinkingBlock      = "\ESC[0 q"
  show SteadyBlock        = "\ESC[2 q"
  show BlinkingUnderline  = "\ESC[3 q"
  show SteadyUnderline    = "\ESC[4 q"
  show BlinkingBar        = "\ESC[5 q"
  show SteadyBar          = "\ESC[6 q"


newtype CursorConfig = CursorConfig
  { cursorShape :: CursorShape
  }

instance Def CursorConfig where
  def = CursorConfig { cursorShape = BlinkingBar }




data State = InputOutput

data ShellProcess = ShellProcess {
    shellConfig :: ShellConfig
  , shellState  :: State
  , shellCache  :: Cache T.Text (Cache T.Text Dynamic)
}


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

defaultHistoryFile :: IO FilePath
defaultHistoryFile = getHomeDirectory <&> (</> ".fok_history")
defaultExitHook :: Hook
defaultExitHook proc = do
  defaultHistoryFile >>= \x -> writeFile x $ T.unpack $ T.strip $ T.intercalate "\n" $ T.strip <$> reverse (history (shellConfig proc))
  putStrLn "\nexit"
  pure True

defaultClearHook :: Hook
defaultClearHook proc = do
  if lastEvent (shellConfig proc) == trigger (shellConfig proc) then
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

data Swallow = Never | Swallowed (IO T.Text)
data Prompt  = SingleLine (IO T.Text) Swallow | MultiLine (IO [T.Text]) Swallow

type PromptGetter = ColorScheme -> Prompt


instance Def [(KeyEvent, Action)] where
  def = [
        ((control, Character "c"), \proc -> haltHook (hooks (shellConfig proc)) proc >>= \x -> if x then haltAction proc else pure proc)
      , ((control, Character "d"), \proc -> exitHook (hooks (shellConfig proc)) proc >>= \x -> if x then exitAction proc else pure proc)
      , ((control, Character "l"), \proc -> clearHook(hooks (shellConfig proc)) (proc {shellConfig = (shellConfig proc) {trigger=(control, Character "l")}}) >>= \x -> if x then clearAction proc else pure proc)
    ]


data ShellConfig = ShellConfig
  { hooks       :: ShellHooks
  , prompt      :: PromptGetter
  
  , colorScheme :: ColorScheme

  , cursorLoc   :: Int                  -- from the right, surprisingly
--, cursor      :: CursorConfig
  , input       :: T.Text
  , binds       :: [(KeyEvent, Action)]
  , lastEvent   :: KeyEvent
  , trigger     :: KeyEvent             -- this should never be overriden globally, locally it should be overwritten with the keyevent trigger (example at ^L handling)
  , jobManager  :: JobMgr

  -- todo: extract into a separate Object, just like ColorSchemes and Autocomplete. Add settings such as ignore duplicates etc.
  , history     :: [T.Text]
  , historyIndex:: Maybe (Int, T.Text)
  , getHistory  :: IO [T.Text]
  
  , builtins    :: [(T.Text, Action)]

  , autocomplete:: AutocompleteConfig
  , cursorConfig:: CursorConfig
  }

readHistory :: IO FilePath -> IO [T.Text]
readHistory f2 = f2 >>= (\f -> fileExist f >>= \x -> unless x (void $ trace ("creating a history file: `" ++ f ++ "`") $ createFile f (ownerReadMode B..|. ownerWriteMode) >>= closeFd) >> TIO.readFile f <&> reverse . T.split (=='\n'))


haltAction :: Action
haltAction proc = let config = shellConfig proc in displayPrompt (prompt config  $ colorScheme config) $> proc {shellConfig = config {input = ""}}

exitAction :: Action
exitAction (ShellProcess {}) = exitSuccess

clearAction :: Action
clearAction proc = let config = shellConfig proc in putStrLn "\ESC[2J\ESC[H" *> displayPrompt (prompt config $ colorScheme config) $> proc

displayPrompt :: Prompt -> IO ()
displayPrompt = \case 
  SingleLine text _ -> eputStrf text
  MultiLine text _  -> eputStrf $ text <&> T.intercalate "\n" 

instance Def ShellConfig where
  def = ShellConfig
    { hooks = def
    , prompt = const $ SingleLine (getFormattedDirectory <&> (<> " > ")) Never
    , input = ""

    , cursorLoc = 0

    , colorScheme = def
    , binds = def
    , lastEvent = (KeyModifiers 0, Escape)
    , trigger = (KeyModifiers 0, Escape)
    , jobManager = JobMgr []
    , history = []
    , historyIndex = Nothing
    , getHistory = readHistory defaultHistoryFile

    , autocomplete = def
    , cursorConfig = def
    }
