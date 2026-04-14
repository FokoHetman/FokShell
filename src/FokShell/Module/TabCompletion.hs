{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module FokShell.Module.TabCompletion where
import FokShell.Module

import Lib.Keys

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Parser
import Lib.Config
import Lib.Primitive
import Data.Maybe (fromMaybe)
import Data.Dynamic (fromDynamic)
import Lib.Format
import System.IO

import Data.Functor
import Debug.Trace (traceShow)
import Data.Bool (bool)
import Control.Monad (when)
import Lib.ColorScheme

import Data.Maybe (isJust, fromMaybe, fromJust)
import Data.Functor
import System.Directory (findExecutable, getDirectoryContents, getPermissions, Permissions (readable), doesDirectoryExist)
import Control.Monad (when, unless)

import System.FilePath.Posix ((</>), takeDirectory)
import Data.Char (isSpace)
import Data.Bool (bool)
import Control.Arrow (Arrow(first))
import Data.List (singleton)

import Language.Parser
import System.Environment (lookupEnv, getEnvironment)
import Data.Bifunctor (Bifunctor(bimap, second))


data TabContextMode = Disabled | Selection deriving Eq
data TabCompletion = TabCompletion
  { mode        :: TabContextMode
  , selected    :: Maybe Int
  , completions :: [T.Text]
  , autocomplete :: AutocompleteConfig
  }

instance Def TabCompletion where
  def = TabCompletion
    { mode = Disabled
    , selected = Nothing
    , completions = []
    }

cleanPrevious :: T.Text -> IO ()
cleanPrevious inp = moveCursor DRight (T.length inp) >> putStr "\ESC[0J" >> hFlush stdout >> moveCursor DLeft (T.length inp)

displayCompletions :: T.Text -> [T.Text] -> Maybe Int -> IO ()
displayCompletions current completions selected = do
  -- setup
  moveCursor DLeft $ curLen + leftLen
  moveCursor Down 1
  -- display
  display completions selected
  -- restore
  moveCursor DLeft $ maxLen + rightLen - curLen
  moveCursor Up $ length completions + 1
  where
    curLen = T.length current
    left = "| "
    right = " |"
    leftLen = T.length left
    rightLen = T.length right
    maxLen = maximum $ fmap T.length completions
    display :: [T.Text] -> Maybe Int -> IO ()
    display [] _ = putStr ['-' | _<-[1..maxLen+leftLen+rightLen]]
    display (x:xs) i = T.putStr (left <> bool "" "\x1b[38;2;255;0;0m" (i==Just 0) <> x <> bool "" "\x1b[0m" (i==Just 0) <> T.pack [' ' | _<- [1..maxLen-T.length x]] <> right) >> moveCursor DLeft (maxLen+leftLen+rightLen) >> moveCursor Down 1 >> display xs ((\x -> x-1) <$> i)


instance Module' TabCompletion ShellProcess where
  initHook' tc p = pure (tc, p)
  preHook' tc p e = case tc.mode of
    Disabled -> case e of
      (KeyModifiers 0, Tab) -> case tc.completions of
        [] -> pure (True, (tc,p))
        [x] -> (False,) . (tc,) <$> replaceCurrentIO x p
        x -> do
          displayCompletions (curWord p.shellConfig) x tc.selected
          pure (False, (tc {mode = Selection, completions = x, selected = Just 0 {- len is at least 2 -}}, p))
      _ -> pure (True, (tc,p))
    Selection -> cleanPrevious p.shellConfig.input >> case e of
      (KeyModifiers 0, Enter) -> case tc.selected of
        Just x -> (False,) . (tc {mode = Disabled, selected = Nothing},) <$> replaceCurrentIO (tc.completions !! x) p
        Nothing -> pure (True, (tc {mode = Disabled, selected = Nothing},p))
      (KeyModifiers 0, Tab) -> case tc.completions of
        [] -> pure (True, (tc,p))
        [x] -> (False,) . (tc,) <$> replaceCurrentIO x p
        x -> do
          let sel = case tc.selected of
                  Just a -> Just $ bool (a+1) 0 (a+1==length tc.completions)
                  Nothing -> Just 0
          displayCompletions (curWord p.shellConfig) x sel
          pure (False, (tc {mode = Selection, completions = x, selected = sel}, p))
      _ -> model (tc.autocomplete) (moddata p) >>= (\case
        [] -> pure (True, (tc {selected = Nothing},p))
        x -> do
          displayCompletions (curWord p.shellConfig) x Nothing
          pure (True, (tc {selected = Nothing},p))
        ) . fst
    where
      conf = p.shellConfig
      curWord c = case runParser parseSeq c.input of
        Just (_,n) -> (\(_,c',_,_) -> c') $ extractData' n c.input c.cursorLoc
        Nothing    -> ""

      replaceCurrentIO :: T.Text -> ShellProcess -> IO ShellProcess
      replaceCurrentIO with proc = moveCursor DLeft (T.length $ curWord conf) >> T.putStr with >> hFlush stdout $> proc {shellConfig = replaceCurrent with conf}
        where
          conf = proc.shellConfig

      replaceCurrent :: T.Text -> ShellConfig -> ShellConfig
      replaceCurrent with c = c {input = ninput}
        where
        t = input c
        i = cursorLoc c
        curword = curWord c

        left = T.take (T.length t - T.length curword - i) t
        right = T.reverse $ T.take i $ T.reverse t
        
        ninput =  left <> with <> right
  postHook' tc p = model (tc.autocomplete) (moddata p) >>= \x -> (cleanPrevious conf.input >> when (tc.mode == Selection) (displayCompletions (curWord conf) (fst x) tc.selected) $> (tc {completions = fst x}, p))
    where
      conf = p.shellConfig
      curWord c = case runParser parseSeq c.input of
        Just (_,n) -> (\(_,c',_,_) -> c') $ extractData' n c.input c.cursorLoc
        Nothing    -> ""

moddata :: ShellProcess -> AutocompleteModelData
moddata p = AutocompleteModelData {modelInput = input c, aColorScheme = colorScheme c, cursorLocation = cursorLoc c,
              historyL = history c, executableList = executablelist' p, builtinNames = fmap fst (builtins c), 
              modelOutput = ([],[]), mCompletionRules = completionRules c} where c = p.shellConfig
executablelist' :: ShellProcess -> [T.Text]
executablelist' p = maybe [] (fromMaybe [] . fromDynamic) (lookupCache (shellCache p) "executables" >>= \x -> lookupCache x "execs")





countMultiple :: T.Text -> T.Text -> Int
countMultiple w t
            | T.null t = 0
            | T.elem (T.head t) w = 1 + countMultiple w (T.tail t)
            | otherwise = countMultiple w $ T.tail t
extractData :: Node -> Int -> (Node, T.Text, Int, [T.Text])
extractData (ProcessCall e args) c = (ProcessCall e args, l!!currentI, index, take currentI l)
  where
    l = fmap nodeToString $ e:args
    (currentI, index) = findCurrent l c
    findCurrent [_] c = (0, c)
    findCurrent (x:xs) c = bool (0, c) (first (1 +) $ findCurrent xs ( c - T.length x)) (T.length x < c)
extractData (Sequence left right) c = bool (extractData left c) (extractData right (c - nlength left - 1)) (c > nlength left)
extractData (And left right) c = bool (extractData left c) (extractData right (c - nlength left - 2)) (c > nlength left)
extractData (Pipe ps left right) c = bool (extractData left c) (extractData right (c - nlength left - pipelength ps)) (c > nlength left)


extractData' :: Node -> T.Text -> Int -> (Node, T.Text, Int, [T.Text])
extractData' n t c = extractData n c'
  where
    leftInput = T.take (T.length t - c) t
    wsCount = countMultiple " '\"" leftInput
    c' = T.length t - c - wsCount

languageModel :: AutocompleteModel
languageModel mdata = case runParser parseSeq input of
  Just (_,n) -> do
    let ((ProcessCall e args), curArg, curInd, prevArgs) = extractData n cursor'
    let rule = lookupRule (nodeToString e) mdata.mCompletionRules
    argMatches <- case rule of
          Just r -> case prevArgs of
            [] -> case filter (T.isPrefixOf curArg) execs of
              [] -> pure []
              x  -> pure x
            (_exec:xs) -> fmap (\(CompRule e _) -> e) <$> nestNTimes r (xs ++ [curArg]) (length xs)
          Nothing -> fileMatches curArg
    pure (argMatches, [])
  Nothing -> pure ([],[])
  where
    input = mdata.modelInput
    loc = T.length input - mdata.cursorLocation
    leftInput = T.take loc input
    wsCount = countMultiple " '\"" leftInput
    -- | cursor independent of whitespace, perfect for use with my Parser
    cursor' = loc - wsCount
    execs = mdata.executableList ++ mdata.builtinNames

    fileMatches exec = let 
        d = takeDirectory (T.unpack exec)
      in ((&&) <$> doesDirectoryExist d <*> (getPermissions d <&> readable)) >>= 
        bool (pure []) (getDirectoryContents d <&> filter (T.isPrefixOf exec) . (bool id (T.pack . (d</>) . T.unpack) (T.pack d `T.isPrefixOf` exec) <$>) . fmap T.pack)

languageHook :: AutocompleteModelData -> IO ()
languageHook = undefined

languageFullRedraw :: AutocompleteModelData -> IO ()
languageFullRedraw = undefined
type AutocompleteModel = AutocompleteModelData -> IO ([T.Text], [T.Text])
data AutocompleteModelData = AutocompleteModelData {
    modelInput      :: T.Text
  , cursorLocation  :: Int
  , aColorScheme    :: ColorScheme
  , modelOutput     :: ([T.Text], [T.Text])
  , builtinNames    :: [T.Text]
  , executableList  :: [T.Text]
  , historyL        :: [T.Text]
  , mCompletionRules:: [CompletionRule]
}

data AutocompleteConfig = AutocompleteConfig {
    model      :: AutocompleteModel
  , redrawHook :: AutocompleteModelData -> IO ()
  , fullRedraw :: AutocompleteModelData -> IO ()
  }

instance Def AutocompleteConfig where
  def = AutocompleteConfig {
    model = languageModel
  , redrawHook = languageHook
  , fullRedraw = languageFullRedraw
  }
