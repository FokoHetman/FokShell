{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module FokShell.Module.TabCompletion where
import FokShell.Module

import Lib.Keys

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Parser
import Lib.Config
import Language.Autocomplete
import Lib.Primitive
import Data.Maybe (fromMaybe)
import Data.Dynamic (fromDynamic)
import Lib.Format
import System.IO

import Data.Functor
import Debug.Trace (traceShow)
import Data.Bool (bool)
import Control.Monad (when)

data TabContextMode = Disabled | Selection deriving Eq
data TabCompletion = TabCompletion
  { mode        :: TabContextMode
  , selected    :: Maybe Int
  , completions :: [T.Text]
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
      _ -> model (conf.autocomplete) (moddata p) >>= (\case
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
  postHook' tc p = model (conf.autocomplete) (moddata p) >>= \x -> (cleanPrevious conf.input >> when (tc.mode == Selection) (displayCompletions conf.input (fst x) tc.selected) $> (tc {completions = fst x}, p))
    where
      conf = p.shellConfig

moddata :: ShellProcess -> AutocompleteModelData
moddata p = AutocompleteModelData {modelInput = input c, aColorScheme = colorScheme c, cursorLocation = cursorLoc c,
              historyL = history c, executableList = executablelist' p, builtinNames = fmap fst (builtins c), 
              modelOutput = ([],[]), mCompletionRules = completionRules c} where c = p.shellConfig
executablelist' :: ShellProcess -> [T.Text]
executablelist' p = maybe [] (fromMaybe [] . fromDynamic) (lookupCache (shellCache p) "executables" >>= \x -> lookupCache x "execs")
