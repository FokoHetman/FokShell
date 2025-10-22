{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module ExposedTypes where

import qualified Data.Text as T
import qualified Data.Bits as B


import System.Exit (exitSuccess, ExitCode (ExitSuccess, ExitFailure))
import Control.Monad (when, unless)
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Posix (getEffectiveUserName, getEnv, fileExist, createFile, ownerWriteMode, setFileMode, ownerReadMode)

import System.FilePath ((</>))

import Data.Functor
import System.IO (hFlush, stdout, IOMode)

import Network.HostName

import GHC.IO.Handle
import System.Process (Pid)
import Debug.Trace (trace)
import Control.Arrow (Arrow(second, first))
import Data.Bifunctor (Bifunctor(bimap))

import Lib.Primitive
import Lib.ColorScheme

-- DISPLAY FUNCTIONS
eputStrf :: IO T.Text -> IO ()
eputStrf t = t >>= \x -> putStr (T.unpack x) <> hFlush stdout

putStrf :: T.Text -> IO ()
putStrf t = putStr (T.unpack t) <> hFlush stdout

-- COLOR SCHEME DATA, move to separate file





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
exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess     = 0
exitCodeToInt (ExitFailure c) = c


newtype JobMgr = JobMgr [Job]


data StringComplex = Basic T.Text | EnvVar T.Text | Variant [StringComplex] | Combination [StringComplex]
  deriving (Show,Eq)

complexToText :: StringComplex -> IO T.Text
complexToText (Basic t) = pure t
complexToText (EnvVar t) = getEnv (T.unpack t) >>= \case
                            Just x -> pure $ T.pack x
                            Nothing -> pure  T.empty
complexToText (Variant ts) = undefined--T.unwords $ fmap complexToText ts
complexToText (Combination ts) = undefined


type Executable = StringComplex
type Args       = [StringComplex]

type FileName = (IO T.Text)

data PipeType = File FileName IOMode | Terminal

displayPipeType :: PipeType -> IO T.Text
displayPipeType Terminal = pure "Terminal"
displayPipeType (File fname mode) = fname >>= \x -> pure $ T.concat [x, "[", T.pack $ show mode, "]"]

displayHide :: PipeType -> T.Text
displayHide Terminal = "Terminal"
displayHide (File _ mode) = T.concat ["File ? ", T.pack $ show mode]

type Condition = (Int -> IO Bool)
data Task = Task {condition :: Condition, body :: Task, next :: Maybe Task, stdinT :: PipeType, stdoutT :: PipeType, stderrT :: PipeType} | PCall Executable Args

instance Show Task where
  show (PCall _ a) = "`" ++ "hidden behind IO"{-T.unpack (complexToText e)-} ++ " [" ++ (T.unpack . T.unwords) (fmap (const "hidden behind IO") a) ++ "]`"
  show (Task _ t n sin sout serr) = "{" ++ T.unpack (displayHide sin) ++ "}c -> " ++ show t ++ case n of
    Just x -> "=>" ++ show x
    Nothing -> ""
    ++ "-->" ++ T.unpack (displayHide sout)

displayTask :: Task -> IO T.Text
displayTask (Task c t n sin out serr) = case n of 
    Just x -> displayTask x >>= \y -> pure $ T.concat ["=>", y]
    Nothing -> pure "" 
  >>= \x -> displayTask t >>= \t -> displayPipeType sin >>= \sin -> displayPipeType out >>= \sout -> pure $ T.concat ["{", sin, "}c->", t, x, "-->", sout
  ]
displayTask (PCall e a) = mapM complexToText a >>= \as -> complexToText e >>= \es -> pure $ T.concat ["`", es, " [", T.unwords as, "]`"]

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

defaultHistoryFile :: IO FilePath
defaultHistoryFile = getHomeDirectory <&> (</> ".fok_history")
defaultExitHook :: Hook
defaultExitHook (ShellProcess c _) = do
  defaultHistoryFile >>= \x -> writeFile x $ T.unpack $ T.intercalate "\n" $ history c
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

data Swallow = Never | Swallowed (IO T.Text)
data Prompt  = SingleLine (IO T.Text) Swallow | MultiLine (IO [T.Text]) Swallow


getFormattedDirectory :: IO T.Text
getFormattedDirectory = do
  dir <- getCurrentDirectory
  home <- getHomeDirectory
  pure $ T.replace (T.pack home) "~" (T.pack dir)


-- TODO: extract to a separate file
data ShellConfig = ShellConfig
  { hooks       :: ShellHooks
  , prompt      :: Prompt
  
  , colorScheme :: ColorScheme

  , cursorLoc   :: Int                  -- from the right, surprisingly
--, cursor      :: CursorConfig
  , input       :: T.Text
  , binds       :: [(KeyEvent, Action)]
  , lastEvent   :: KeyEvent
  , trigger     :: KeyEvent             -- this should never be overriden globally, locally it should be overwritten with the keyevent trigger (example at ^L handling)
  , jobManager  :: JobMgr
  , history     :: [T.Text]
  , historyIndex:: Maybe (Int, T.Text)
  , getHistory  :: IO [T.Text]
  }

data State = InputOutput

data ShellProcess = ShellProcess ShellConfig State


instance Def ShellConfig where
  def = ShellConfig
    { hooks = def
    , prompt = SingleLine (getFormattedDirectory <&> (<> " > ")) Never
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
    }

readHistory :: IO FilePath -> IO [T.Text]
readHistory f2 = f2 >>= (\f -> fileExist f >>= \x -> unless x (void $ trace ("creating a history file: `" ++ f ++ "`") $ createFile f (ownerReadMode B..|. ownerWriteMode)) >> readFile f <&> (T.split (=='\n') . T.pack))

{- better solution found: IO T.Text
shortcuts :: [(T.Text, IO T.Text)]
shortcuts = [("%u", T.pack <$> getEffectiveUserName), ("%d", getFormattedDirectory), ("%h", T.pack <$> getHostName)]
-}

replaceShortcuts :: [(T.Text, IO T.Text)] -> T.Text -> IO T.Text
replaceShortcuts (x:xs) t = snd x >>= \y -> replaceShortcuts xs (T.replace (fst x) y t)
replaceShortcuts [] text = pure text

clearLines :: Direction -> Int -> IO ()
clearLines _ 0 = putStr "\ESC[2K\r"
clearLines d i = putStr "\ESC[2K\r" >> moveCursor d 1 >> clearLines d (i-1)

swallowPrompt :: Int -> T.Text -> Prompt -> IO ()
swallowPrompt c input = \case
  SingleLine _ (Swallowed sw) -> putStr "\ESC[2K\r" >> clearLines Up 0 >> (sw >>= putStrf) >> putStrf (T.strip input) >> moveCursor DLeft c
  MultiLine  t2 (Swallowed sw) -> t2 >>= \t -> clearLines Up (length t - 1) >> (sw >>= putStrf) >> putStrf (T.strip input) >> moveCursor DLeft c
  SingleLine _ Never -> pure ()
  MultiLine _ Never -> pure ()


displayPrompt :: Prompt -> IO ()
displayPrompt = \case 
  SingleLine text _ -> eputStrf text
  MultiLine text _  -> eputStrf $ text <&> T.intercalate "\n" 



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


moveCursor :: Direction -> Int -> IO ()
moveCursor _ 0 = pure ()
moveCursor DLeft i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "D"
moveCursor DRight i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "C"
moveCursor Up i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "A"
moveCursor Down i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "B"



moveCursor':: ShellConfig -> Direction -> Int -> IO ()
moveCursor' c DLeft  i = when (T.length (input c) > cursorLoc c) (moveCursor DLeft i)
moveCursor' c DRight i = when (cursorLoc c > 0)  (moveCursor DRight i)
moveCursor' _ _ _ = error "unsupported '-wrapped direction"


updateWithKey :: KeyEvent -> ShellProcess -> ShellProcess
updateWithKey event (ShellProcess conf state) = ShellProcess conf {lastEvent=event} state


