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

import System.Posix (fileExist, createFile, ownerWriteMode, ownerReadMode, closeFd, changeWorkingDirectory, getFileStatus, isDirectory, isRegularFile)
import System.Directory (getHomeDirectory, doesDirectoryExist, getPermissions, Permissions (readable, executable, searchable), getDirectoryContents, doesFileExist, doesPathExist)

import System.FilePath ((</>), takeDirectory)

import Debug.Trace

import Data.Functor

import GHC.IO.Handle
import Language.Parser
import GHC.IO.Exception (ExitCode)
import Control.Monad
import System.Exit (exitSuccess)
import Data.List (sort, group, intersperse)
import Control.Concurrent (threadDelay)
import Data.Bool (bool)

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

type Builtin = (T.Text, [T.Text] -> Action)

cd :: Builtin
cd = ("cd", \args process -> do
    _ <- case args of
      [x] -> do
        let d = T.unpack x
        doesDirectoryExist d >>= bool (putStrf "cd: directory does not exist.\n") (getPermissions d >>= bool (putStrf "cd: no permissions.\n") (changeWorkingDirectory d) . searchable)
      []  -> putStrf "cd: no arg provided.\n"
      _   -> putStrf "cd: too many args provided.\n"
    pure process-- replace getters with just a cached value that changes here?
  )

animateMovement :: [String] -> IO ()
animateMovement x = putStrLn "" >> f x (length (head x))
  where
    f :: [String] -> Int -> IO ()
    f x 0 = putStrf' ("\ESC[" ++ show (length x) ++ "B")
    f x i = f' x i >> putStrf' ("\ESC[" ++ show (length x) ++ "A") >> threadDelay 50000 >> f x (i-1)
    f' :: [String] -> Int -> IO ()
    f' (x:xs) i = putStrLn (reverse $ take (length x - i) $ reverse x) >> f' xs i
    f' [] _ = pure ()
count :: Eq a => [a] -> a -> Int
count xs find = length (filter (== find) xs)

dedup :: Ord a => [a] -> [a]
dedup = map head . group . sort

wrapped :: Builtin
wrapped = ("wrapped", \_ process -> do
    let conf = shellConfig process
    let hist = history conf
    let mis_ls = count hist "sl"
    let mis_nvim = sum $ fmap (count hist . T.pack) $ filter (/="nvim") $ dedup [a:b:c:[d] | a<-"nvim", b<-"nvim", c<-"nvim", d<-"nvim"]
    
    
    animateMovement [
        "\ESC" ++ concat (intersperse "*====" ["\ESC[38;5;" ++ show id ++ "m" | id<- [0..20]]) ++ "*\ESC[0m" --"*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*"
      ,  "                                                          \ESC[1m==============*FokShell Wrapped*=====\ESC[0m"
      ,  "                                      \ESC[1mMisspelled `ls` " ++ show mis_ls ++ " times\ESC[0m"
      ,  "               \ESC[1mMisspelled `nvim` " ++ show mis_nvim ++ " times\ESC[0m"
      , "\ESC" ++ concat (intersperse "*====" ["\ESC[38;5;" ++ show id ++ "m" | id<- [0..20]]) ++ "*\ESC[0m"
      --, "*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*====*"
      , "\xE001\xE002\xE003\xE004\xE005\xE006\xE007\xE008\xE009\xE00A\xE00B\xE00C\xE00D\xE00E\xE00F\xE010\xE011\xE012\xE013\xE014\xE015\xE016\xE017\xE018\xE019\xE01A\xE01B\xE01C\xE01D\xE01E\xE01F\xE020\xE021\xE022\xE023\xE024\xE025\xE026\xE027\xE028\xE029\xE02A\xE02B\xE02C\xE02D\xE02E\xE02F\xE030\xE031\xE032\xE033\xE034\xE035\xE036\xE037\xE038\xE039\xE03A\xE03B\xE03C\xE03D\xE03E\xE03F\xE040\xE041\xE042\xE043\xE044\xE045\xE046\xE047\xE048\xE049\xE04A\xE04B\xE04C\xE04D\xE04E\xE04F\xE050\xE051\xE052\xE053\xE054\xE055\xE056\xE057\xE058\xE059\xE05A\xE05B\xE05C\xE05D\xE05E\xE05F\xE060"
      ]

    pure process
  )


readHistory :: IO FilePath -> IO [T.Text]
readHistory f2 = f2 >>= (\f -> fileExist f >>= \x -> unless x (void $ trace ("creating a history file: `" ++ f ++ "`") $ createFile f (ownerReadMode B..|. ownerWriteMode) >>= closeFd) >> TIO.readFile f <&> reverse . T.split (=='\n'))


haltAction :: Action
haltAction proc = let config = shellConfig proc in displayPrompt (prompt config  $ colorScheme config) $> proc {shellConfig = config {input = ""}}

exitAction :: Action
exitAction (ShellProcess {}) = exitSuccess

-- BUG: it doesn't display the current input, making clear with prompt yield weird results
clearAction :: Action
clearAction proc = let config = shellConfig proc in putStrLn "\ESC[2J\ESC[H" *> displayPrompt (prompt config $ colorScheme config) $> proc

displayPrompt :: Prompt -> IO ()
displayPrompt = \case 
  SingleLine text _ -> eputStrf text
  MultiLine text _  -> eputStrf $ text <&> T.intercalate "\n"



{-
nix :: CompletionRule
nix = CompRule "nix" (\t -> pure $ filter (\(CompRule i _) -> t `T.isPrefixOf` i) [
    CompRule "run" flake
  ])
  where
    flake :: T.Text -> IO [CompletionRule]
    flake t = case T.split (=='#') t of
      [x] -> (++) <$> directoryRules <*> registries
-}


cdCompletion :: CompletionRule
cdCompletion = CompRule "cd" $ fileCompletion ((<&> isDirectory) . getFileStatus) $ const (pure [])

instance Def [CompletionRule] where
  def = [
      --nix
      cdCompletion
    , fileListCompletion ((<&> isRegularFile) . getFileStatus) "cat"
    ]

instance Def [Builtin] where
  def = [
      cd
    , wrapped
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
  
  , builtins    :: [Builtin]

  , autocomplete:: AutocompleteConfig
  , cursorConfig:: CursorConfig

  , completionRules :: [CompletionRule]
  }

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

    , builtins = def

    , autocomplete = def
    , cursorConfig = def
    , completionRules = def
    }


