{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module FokShell.Module.TabCompletion where
import FokShell.Module
import FokShell.Module.History

import Lib.Keys

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Parser
import FokShell.Types
import Lib.Primitive
import Data.Maybe (fromMaybe)
import Data.Dynamic (fromDynamic)
import Data.Map qualified as Map
import Lib.Format
import System.IO

import Data.Functor
import Data.Bool (bool)
import Control.Monad (when, filterM)

import Data.Proxy
import Control.Arrow (Arrow(first))
import System.Directory (getDirectoryContents, getPermissions, Permissions (readable), doesDirectoryExist)
import System.FilePath.Posix ((</>), takeDirectory)
import Debug.Trace (traceShow)
import Data.List (sort)
import FokShell.Module.Parser


data TabContextMode = Disabled | Selection deriving (Eq, Show)
data TabCompletion = TabCompletion
  { mode        :: TabContextMode
  , selected    :: Maybe Int
  , completions :: [T.Text]
  , sortAlgorithm   :: ShellProcess -> [T.Text] -> [T.Text]
  , autocomplete    :: AutocompleteConfig
  , maxSuggestions  :: Int
  , shadowText      :: Bool
  , completionRules :: [CompletionRule]
  }

instance Def TabCompletion where
  def = TabCompletion
    { mode = Disabled
    , selected = Nothing
    , completions = []
    , sortAlgorithm = const sort
    , autocomplete = def
    , maxSuggestions = 10
    , shadowText = True
    , completionRules = []
    }

cleanPrevious :: T.Text -> IO ()
cleanPrevious inp = T.putStr (moveCursorRaw DRight (T.length inp) <> "\ESC[0J" <> moveCursorRaw DLeft (T.length inp)) >> hFlush stdout

displayCompletions :: T.Text -> [T.Text] -> Maybe Int -> Int -> IO ()
displayCompletions _ [] _ _ = pure ()
displayCompletions current (x:xs) selected displayed = do
  T.putStr $
    -- setup
    moveCursorRaw DLeft (curLen + leftLen) <> moveCursorRaw Down 1
    -- display
    <> display completions ((\x -> x-prev) <$> selected)
    -- restore
    <> moveCursorRaw DLeft (maxLen + rightLen - curLen)
    <> moveCursorRaw Up (length completions + 1)
  hFlush stdout
  where
    prev = case selected of
      Just s -> (s `div` displayed)*displayed
      Nothing -> 0
    completions = take displayed $ drop prev $ x:xs
    curLen = T.length current
    left = "| "
    right = " |"
    leftLen = T.length left
    rightLen = T.length right
    maxLen = maximum $ fmap T.length completions
    display :: [T.Text] -> Maybe Int -> T.Text
    display [] _ = T.pack ['-' | _<-[1..maxLen+leftLen+rightLen]]
    display (x:xs) i = left <> bool "" "\x1b[38;2;255;0;0m" (i==Just 0) <> x <> bool "" "\x1b[0m" (i==Just 0) <> T.pack [' ' | _<- [1..maxLen-T.length x]] <> right
                        <> moveCursorRaw DLeft (maxLen+leftLen+rightLen) <> moveCursorRaw Down 1 <> display xs ((\x -> x-1) <$> i)


instance Module' TabCompletion ShellProcess where
  initHook' tc p = pure (tc, p)
  preHook' tc p e = case tc.mode of
    Disabled -> case e of
      (KeyModifiers 0, Tab) -> case tc.completions of
        [] -> pure (True, (tc,p))
        [x] -> (False,) . (tc,) <$> replaceCurrentIO x p
        x -> do
          displayCompletions (curWord p.shellConfig) (sort x) tc.selected tc.maxSuggestions
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
          displayCompletions (curWord p.shellConfig) (sort x) sel tc.maxSuggestions
          pure (False, (tc {mode = Selection, completions = x, selected = sel}, p))
      _ -> (tc.autocomplete.model) p tc >>= (\case
        [] -> pure (True, (tc {selected = Nothing},p))
        x -> do
          displayCompletions (curWord p.shellConfig) (sort x) Nothing tc.maxSuggestions
          pure (True, (tc {selected = Nothing},p))
        ) . fst
    where
      conf = p.shellConfig
      parser = case requestModule (Proxy @ParserModule) conf.modules of
        (x:_) -> x
        _ -> def
      curWord c = case runParser parser.parser c.input of
        Just (_,n) -> (\(_,c',_) -> head c') $ getRawDataWrapped n c.input c.cursorLoc
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
  postHook' tc p _ = tc.autocomplete.model p tc >>= \x -> (when (tc.mode == Selection) (cleanPrevious conf.input >> displayCompletions (curWord conf) (fst x) tc.selected tc.maxSuggestions) $> (True, (tc {completions = fst x}, p)))
    where
      conf = p.shellConfig
      parser = case requestModule (Proxy @ParserModule) conf.modules of
        (x:_) -> x
        _ -> def
      curWord c = case runParser parser.parser c.input of
        Just (_,n) -> (\(_,c',_) -> head c') $ getRawDataWrapped n c.input c.cursorLoc
        Nothing    -> ""
  exitHook' tc p = pure (tc, p)
{-moddata :: ShellProcess -> AutocompleteModelData
moddata p = AutocompleteModelData {modelInput = input c, aColorScheme = colorScheme c, cursorLocation = cursorLoc c,
              historyL = concatMap (\x -> x.history) $ requestModule (Proxy @HistoryModule) $ c.modules, executableList = executablelist' p, builtinNames = fmap fst (builtins c), 
              modelOutput = ([],[]), mCompletionRules = completionRules c} where c = p.shellConfig
-}
executablelist' :: ShellProcess -> [T.Text]
executablelist' p = maybe [] (fromMaybe [] . fromDynamic) (lookupCache (shellCache p) "executables" >>= \x -> lookupCache x "execs")





countMultiple :: T.Text -> T.Text -> Int
countMultiple w t
            | T.null t = 0
            | T.elem (T.head t) w = 1 + countMultiple w (T.tail t)
            | otherwise = countMultiple w $ T.tail t

getRawDataWrapped :: Node -> T.Text -> Int -> (Node, [T.Text], Int)
getRawDataWrapped n t c = getRawData n c'
  where
    leftInput = T.take (T.length t - c) t
    wsCount = countMultiple " '\"" leftInput
    c' = T.length t - c - wsCount



languageModel :: AutocompleteModel
languageModel proc@ShellProcess{shellConfig = conf} tc = case runParser parser.parser input of
  Just (_,n) -> do
    let (node, prevArgs, curInd) = getRawData n cursor'
    let curArg = last prevArgs
    case withProxyNode (Proxy @ProcessCall) node of
      Just (ProcessCall e _) -> case prevArgs of
        [] -> case filter (T.isPrefixOf curArg) execs of
          [] -> pure ([],[])
          x  -> pure (x,[])
        (_exec:xs) -> do
          let rule = lookupRule (nodeToText e) tc.completionRules
          argMatches <- case rule of
            Just r -> fmap (\(CompRule e _) -> e) <$> nestNTimes r (xs ++ [curArg]) (length xs)
            Nothing -> fileMatches curArg
          pure (argMatches, [])
      Nothing -> pure ([],[])
  Nothing -> pure ([],[])
  where
    parser = case requestModule (Proxy @ParserModule) conf.modules of
      (x:_) -> x
      _ -> def
    input = conf.input
    loc = T.length input - conf.cursorLoc
    leftInput = T.take loc input
    wsCount = countMultiple " '\"" leftInput
    -- cursor independent of whitespace
    cursor' = loc - wsCount
    execs = dedup $ executablelist' proc ++ fmap fst conf.builtins

    fileMatches exec = let 
        d = takeDirectory (T.unpack exec)
      in (doesDirectoryExist d >>= bool (pure False) (getPermissions d <&> readable)) >>= 
        bool (pure []) (getDirectoryContents d <&> filter (T.isPrefixOf exec) . (bool id (T.pack . (d</>) . T.unpack) (T.pack d `T.isPrefixOf` exec) <$>) . fmap T.pack)

languageHook :: ShellProcess -> IO ()
languageHook = undefined

languageFullRedraw :: ShellProcess -> IO ()
languageFullRedraw = undefined
type AutocompleteModel = ShellProcess -> TabCompletion -> IO ([T.Text], [T.Text])

data AutocompleteConfig = AutocompleteConfig {
    model      :: AutocompleteModel
  , redrawHook :: ShellProcess -> IO ()
  , fullRedraw :: ShellProcess -> IO ()
  }

instance Def AutocompleteConfig where
  def = AutocompleteConfig {
    model = languageModel
  , redrawHook = languageHook
  , fullRedraw = languageFullRedraw
  }


data CompletionRule = CompRule T.Text (T.Text -> IO [CompletionRule])

instance Show CompletionRule where
  show (CompRule x _) = "CompRule `" ++ T.unpack x ++ "`"

unwrapArgs :: CompletionRule -> [T.Text] -> IO [CompletionRule]
unwrapArgs (CompRule _ f) [t] = f t
unwrapArgs (CompRule _ f) (t:ts) = f t >>= \case
    [CompRule x f2] -> if x==t then unwrapArgs (CompRule x f2) ts else pure []
    _ -> pure []
unwrapArgs _ [] = pure []
-- todo: add completions and file cache to this
isValidArgument :: [CompletionRule] -> [T.Text] -> IO Bool
isValidArgument rules (execuset:args') = case lookupRule execuset rules of
  Just (CompRule x f) -> unwrapArgs (CompRule x f) args' <&> \case
    [CompRule x2 _] -> x2==last args'
    _   -> False
  Nothing             -> pure True
isValidArgument _ [] = pure True

lookupRule :: T.Text -> [CompletionRule] -> Maybe CompletionRule
lookupRule t (CompRule x f:xs) = bool (lookupRule t xs) (Just $ CompRule x f) (t==x)
lookupRule _ [] = Nothing


nestNTimes :: CompletionRule -> [T.Text] -> Int -> IO [CompletionRule]
nestNTimes (CompRule _ f) (t:_) 0 = f t
nestNTimes (CompRule _ f) (t:ts) n = f t >>= \case
  [CompRule t2 f2] -> if t==t2 then nestNTimes (CompRule t2 f2) ts (n-1) else pure []
  _ -> pure []
nestNTimes _ [] _ = pure []



fileCompletion :: (FilePath -> IO Bool) -> (T.Text -> IO [CompletionRule]) -> (T.Text -> IO [CompletionRule])
fileCompletion filtre nest t = do
    let d = takeDirectory $ T.unpack t
    exists <- doesDirectoryExist d
    if exists then getPermissions d >>= \x ->
      if readable x then do
        localFiles <- getDirectoryContents d >>= filterM (filtre . (d</>))
        let matches = filter (T.isPrefixOf t) $ bool id (T.pack . (d</>) . T.unpack) (T.pack d `T.isPrefixOf` t) <$> fmap T.pack localFiles
        pure $ fmap (`CompRule` nest) matches
      else pure []
    else pure []
fileCompletionRec :: (FilePath -> IO Bool) -> T.Text -> IO [CompletionRule]
fileCompletionRec filtr = fileCompletion filtr (fileCompletionRec filtr)

fileListCompletion :: (FilePath -> IO Bool) -> T.Text -> CompletionRule
fileListCompletion filtr = (`CompRule` fileCompletionRec filtr)
