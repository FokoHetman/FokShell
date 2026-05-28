{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module FokShell.Module.TabCompletion where
import FokShell.Module

import Lib.Keys

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Parser
import FokShell.Types
import Lib.Primitive
import Data.Maybe (fromMaybe)
import Data.Dynamic (fromDynamic)
import Lib.Format
import System.IO

import Data.Map qualified as Map

import Data.Functor
import Data.Bool (bool)
import Control.Monad (when, filterM)

import Data.Proxy
import System.Directory (getDirectoryContents, getPermissions, Permissions (readable), doesDirectoryExist)
import System.FilePath.Posix ((</>), takeDirectory)
import Data.List (sort)
import FokShell.Module.Parser
import System.Posix (isDirectory, getFileStatus)
import FokShell.Module.Preprocessor (connectPreprocessors)
import Control.Concurrent.STM
import Debug.Trace (traceShow)
import FokShell.Utils (getExecutables)
import FokShell.Module.JobManager

data TabContextMode = Disabled | Selection deriving (Eq, Show)
data TabCompletion = TabCompletion
  { mode        :: TabContextMode
  , selected    :: Maybe Int
  , completions :: [T.Text]
  , sortAlgorithm   :: ShellConfig -> [T.Text] -> [T.Text]
  , autocomplete    :: AutocompleteConfig
  , maxSuggestions  :: Int
  , shadowText      :: Bool
  , completionRules :: [CompletionRule]
  , executables :: [FilePath]
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
    , completionRules = [cdCompletion]
    , executables = []
    }

cleanPrevious :: T.Text -> IO ()
cleanPrevious inp = T.putStr (moveCursorRaw DRight (T.length inp) <> "\ESC[0J" <> moveCursorRaw DLeft (T.length inp)) >> hFlush stdout

getCommonPrefix :: [T.Text] -> T.Text
getCommonPrefix [] = undefined
getCommonPrefix [x] = x
getCommonPrefix (x:y:xs) = getCommonPrefix $ new:xs
  where new = T.pack $ fmap (\(x,_) -> x) $ filter (\(x,y) -> x==y) $ T.zip x y

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


runParser' :: ShellConfig -> T.Text -> IO (Maybe Node)
runParser' conf t = do
  parser <- case requestModule @ParserModule conf.modules of
      (x:_) -> readTVarIO x
      _ -> pure def
  let preprocess = connectPreprocessors parser.preprocessors
  case runParser parser.parser t of
    Nothing -> pure Nothing
    Just (_, x) -> Just <$> preprocess conf x


instance Module' TabCompletion ShellConfig where
  initHook' tc _ = do
    execs <- getExecutables
    atomically $ modifyTVar tc $ \tc' -> tc' {executables = execs}
  exitHook' _ _ = pure ()
  resetHook' tc p = do
    p' <- readTVarIO p
    cleanPrevious p'.input
    atomically $ modifyTVar tc $ \tc' -> tc' {mode = Disabled, selected = Nothing}
  preHook' tc conf e = do
    conf' <- readTVarIO conf
    tc' <- readTVarIO tc
    curWord <- runParser' conf' conf'.input >>= \case
        Just n -> do
          pure $ fromMaybe "" $ (\(_,y,_) -> last y) <$>  getRawDataWrapped n conf'.input conf'.cursorLoc
        Nothing    -> pure ""
    parser <- case requestModule @ParserModule conf'.modules of
        (x:_) -> readTVarIO x
        _ -> pure def
    let curWordRaw c = case runParser parser.parser c.input >>= (\n -> getRawDataWrapped n c.input c.cursorLoc) . snd of
          Just (_,x,_) -> last x
          Nothing    -> ""
        replaceCurrentIO :: T.Text -> TVar ShellConfig -> IO ()
        replaceCurrentIO with conf = do
          c <- readTVarIO conf
          when (T.length (curWordRaw c) > 0) $ do
            let t = input c
                i = cursorLoc c
                left = T.take (T.length t - T.length (curWordRaw c) - i) t
                right = T.reverse $ T.take i $ T.reverse t
                ninput = left <> with <> right

            T.putStr (bool (moveCursorRaw DLeft (T.length $ curWordRaw conf')) "" (T.null (curWordRaw conf')) <> with <> right <> bool (moveCursorRaw DLeft (T.length right)) "" (T.null right))
            hFlush stdout
            atomically $ modifyTVar conf (\conf' -> conf' {input=ninput})
    case tc'.mode of
      Disabled -> case e of
        (KeyModifiers 0, Tab) -> case tc'.completions of
          [] -> pure True
          [x] -> replaceCurrentIO x conf $> False
          x -> do
            let common = getCommonPrefix x
            displayCompletions (curWordRaw conf') (tc'.sortAlgorithm conf' x) tc'.selected tc'.maxSuggestions
            replaceCurrentIO common conf
            atomically . modifyTVar tc $ \tc' -> tc' {mode = Selection, completions = x, selected = Just 0}
            pure False
            --(False,) <$> ((tc {mode = Selection, completions = x, selected = Just 0 {- len is at least 2 -}},) <$> replaceCurrentIO common p)
        _ -> pure True
      Selection -> cleanPrevious conf'.input >> case e of
        (KeyModifiers 0, Enter) -> case tc'.selected of
          Just x -> do
            replaceCurrentIO (tc'.completions !! x) conf
            atomically . modifyTVar tc $ \tc' -> tc' {mode = Disabled, selected = Nothing}
            pure False
          Nothing -> atomically (modifyTVar tc $ \tc' -> tc' {mode = Disabled, selected = Nothing}) $> True
        (KeyModifiers 0, Tab) -> case tc'.completions of
          [] -> pure True
          [x] -> replaceCurrentIO x conf $> False
          x -> do
            let sel = case tc'.selected of
                    Just a -> Just $ bool (a+1) 0 (a+1==length tc'.completions)
                    Nothing -> Just 0
            displayCompletions (curWordRaw conf') (tc'.sortAlgorithm conf' x) sel tc'.maxSuggestions
            atomically . modifyTVar tc $ \tc' -> tc' {mode = Selection, completions = x, selected = sel}
            pure False
        _ -> (tc'.autocomplete.model) conf' tc' >>= (\case
          [] -> do
            atomically . modifyTVar tc $ \tc' -> tc' {selected = Nothing}
            pure True --, (tc {selected = Nothing},p))
          x -> do
            displayCompletions (curWordRaw conf') (tc'.sortAlgorithm conf' x) Nothing tc'.maxSuggestions
            atomically . modifyTVar tc $ \tc' -> tc' {selected = Nothing}
            pure True
          ) . fst
    where

      {-replaceCurrent :: T.Text -> ShellConfig -> ShellConfig
      replaceCurrent with c = c {input = ninput}
        where
        t = input c
        i = cursorLoc c

        left = T.take (T.length t - T.length (curWordRaw c) - i) t
        right = T.reverse $ T.take i $ T.reverse t
        
        ninput =  left <> with <> right-}

  postHook' tc conf _ = do
    conf' <- readTVarIO conf
    tc' <- readTVarIO tc
    curWord <- runParser' conf' conf'.input >>= \case
      Just n -> do
        pure $ fromMaybe "" $ (\(_,y,_) -> last y) <$>  getRawDataWrapped n conf'.input conf'.cursorLoc
      Nothing    -> pure ""

    parser <- case requestModule @ParserModule conf'.modules of
      (x:_) -> readTVarIO x
      _ -> pure def
    let curWordRaw c = case runParser parser.parser c.input >>= (\n -> getRawDataWrapped n c.input c.cursorLoc) . snd of
          Just (_,x,_) -> last x
          Nothing    -> ""
    m <- tc'.autocomplete.model conf' tc'
    when (tc'.mode == Selection) $ do
      cleanPrevious conf'.input
      displayCompletions (curWordRaw conf') (tc'.sortAlgorithm conf' $ fst m) tc'.selected tc'.maxSuggestions
    
    atomically . modifyTVar tc $ \tc'' -> tc'' {completions = tc''.sortAlgorithm conf' $ fst m}

    pure True

--executablelist' :: ShellConfig -> [T.Text]
--executablelist' p = maybe [] (fromMaybe [] . fromDynamic) (lookupCache (shellCache p) "executables" >>= \x -> lookupCache x "execs")

countMultiple :: T.Text -> T.Text -> Int
countMultiple w t
            | T.null t = 0
            | T.elem (T.head t) w = 1 + countMultiple w (T.tail t)
            | otherwise = countMultiple w $ T.tail t

getRawDataWrapped :: Node -> T.Text -> Int -> Maybe (Node, [T.Text], Int)
getRawDataWrapped n t c = getRawData n c'
  where
    leftInput = T.take (T.length t - c) t
    wsCount = countMultiple " '\"" $ T.stripEnd leftInput
    c' = T.length t - c - wsCount

languageModel :: AutocompleteModel
languageModel conf tc = do
  builtinl <- case requestModule @JobManager conf.modules of
      (x:_) -> readTVarIO x <&> \x' -> Map.keys x'.builtins
      [] -> pure []
  let execs = dedup $ builtinl <> fmap T.pack tc.executables
  runParser' conf input >>= \case
    Just n -> do
      let (node, prevArgs, curInd) = case getRawData n cursor' of
            Just x -> x
            Nothing -> (Node $ NodeString "" SingleQuote, [], 0) 
      let curArg = last prevArgs
      case withProxyNode (Proxy @ProcessCall) node of
        Just (ProcessCall e _) -> case init prevArgs of
          [] -> case filter (T.isPrefixOf curArg) execs of
            [] -> pure ([],[])
            x  -> pure (x,[])
          (_exec:xs) -> do
            let rule = lookupRule (nodeToText e) tc.completionRules
            argMatches <- case rule of
              Just r -> fmap (\(CompRule e _) -> e) <$> nestNTimes r (xs ++ [curArg]) (length xs)
              Nothing -> fmap (\(CompRule e _) -> e) <$> fileCompletionRec (const $ pure True) curArg
            pure (argMatches, [])
        Nothing -> pure ([],[])
    Nothing -> pure ([],[])
  where
    input = conf.input
    loc = T.length input - conf.cursorLoc
    leftInput = T.take loc input
    wsCount = countMultiple " '\"" leftInput
    -- cursor independent of whitespace
    cursor' = loc - wsCount

languageHook :: ShellConfig -> IO ()
languageHook = undefined

languageFullRedraw :: ShellConfig -> IO ()
languageFullRedraw = undefined
type AutocompleteModel = ShellConfig -> TabCompletion -> IO ([T.Text], [T.Text])

data AutocompleteConfig = AutocompleteConfig {
    model      :: AutocompleteModel
  , redrawHook :: ShellConfig -> IO ()
  , fullRedraw :: ShellConfig -> IO ()
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
        localFiles' <- mapM (\x -> safeCheck (\y -> isDirectory <$> getFileStatus y) (d</>x) <&> bool x (x <> "/")) localFiles
        let matches = filter (T.isPrefixOf t) $ bool id (T.pack . (d</>) . T.unpack) (T.pack d `T.isPrefixOf` t) <$> fmap T.pack localFiles'
        pure $ fmap (`CompRule` nest) matches
      else pure []
    else pure []
fileCompletionRec :: (FilePath -> IO Bool) -> T.Text -> IO [CompletionRule]
fileCompletionRec filtr = fileCompletion filtr (fileCompletionRec filtr)

fileListCompletion :: (FilePath -> IO Bool) -> T.Text -> CompletionRule
fileListCompletion filtr = (`CompRule` fileCompletionRec filtr)

cdCompletion :: CompletionRule
cdCompletion = CompRule "cd" $ fileCompletion (safeCheck $ (<&> isDirectory) . getFileStatus) $ const (pure [])
