{-# LANGUAGE OverloadedStrings #-}
module Lib.Autocomplete where

import qualified Data.Text as T
import Lib.Primitive
import Lib.Format
import Lib.ColorScheme
import Data.Maybe (isJust, fromMaybe, isNothing)
import Data.Functor
import System.Directory (findExecutable, getDirectoryContents, getPermissions, Permissions (readable), doesDirectoryExist)
import Control.Monad (when, unless)

import System.FilePath.Posix ((</>), takeDirectory)
import Data.Char (isSpace)
import Data.Bool (bool)
import Control.Arrow (Arrow(first))
import Data.Foldable (for_)
import Data.List (singleton)

import Language.Parser
import Debug.Trace (traceShow)

{- input -> cursor location -> history -> most related autocompletes in form of (CurrentWord, WholeQuery) -}
type AutocompleteModel = AutocompleteModelData -> IO ([T.Text], [T.Text])
{- todo: make input a Record, consolidate last 2 args into passing Caches -}

defaultModel :: AutocompleteModel
defaultModel modelData = if isMatchingExecutable then do
    let matches = filter (T.isPrefixOf fstWord) executables
    pure (matches, wholeMatches)
  else do
    let d = takeDirectory $ T.unpack curWord
    --print "///"
    --print d
    --print "///"
    exists <- doesDirectoryExist d
    if exists then getPermissions d >>= \x ->
      if readable x then do
        localFiles <- getDirectoryContents d
        --print "\\\\\\"
        --print localFiles
        --print "\\\\\\"
        let matches = filter (T.isPrefixOf curWord) $ bool id (T.pack . (d</>) . T.unpack) (T.pack d `T.isPrefixOf` curWord) <$> fmap T.pack localFiles
        --print fstWord
        --print curWord
        pure (matches, wholeMatches)
      else
        pure ([], [])
    else
      pure ([], [])
  where
    inp = modelInput modelData
    cursor = cursorLocation modelData
    hist = historyL modelData
    executables = builtinNames modelData ++ executableList modelData

    pathExecutables = mapM executablesInDir =<< getDirsInPath

    (matchIndex, cursorIndex) = extractIndexes inp cursor
    curWord = maybe "" (reverse (T.words inp)!!) matchIndex
    fstWord = fstWord' $ T.words inp
    fstWord' (x:__) = x
    fstWord' [] = ""
    isMatchingExecutable = matchIndex == Just (length (T.words inp)-1)

    wholeMatches = filter (T.isPrefixOf inp) hist


findArg :: [(StringComplex', (T.Text, T.Text))] -> Int -> Maybe (Int, T.Text)
findArg ((str, (a, b)):ts) i = bool (first (+1) <$> findArg ts (i - T.length t - T.length b)) (Just (0,t')) (T.length t >= i && i >= 0)
  where 
    t = a<>complexToRawText' str
    t' = case str of
      Basic s -> s
      Variant s -> case snd <$> findArg s (i - T.length a) of
        Just x -> x
        Nothing -> error "shouldn't happen I think idk"
      Combination _ -> error "idk bruv"
      _ -> error "?"
findArg [] i = Nothing


languageModel :: AutocompleteModel
languageModel modelData = case parsed of
    Just (_, n)  -> case n of
      ProgramCall exec args -> bool
        {-matching args-}
        (argMatches <&> (,[]))
        {-matching executable-}
        (pure (executableMatches (fst exec),[]))
        (T.length exec' >= cursor)
        where
          exec' = complexToRawText' $ fst exec
          exec'' = complexToRawText exec
          executableMatches e = case e of
            Basic b -> filter (T.isPrefixOf b) executables
            Variant (v:vs) -> undefined
            
          rule = lookupRule exec' rules
          curArg = findArg args (cursor - T.length exec'')
          argMatches = case rule of
            Just r -> case curArg of
              Nothing -> pure []
              Just (x,_)  -> fmap (\(CompRule e _) -> e) <$> nestNTimes r (fmap complexToRawText args) x
            Nothing -> case curArg of 
              Just (_,arg) -> fileMatches arg
              Nothing -> pure []
      _ -> error "undefined behavior"
    Nothing  -> pure ([], [])
  where
    inp = modelInput modelData
    cursor = T.length inp - cursorLocation modelData
    parsed = runParser parseExpr inp
    executables = builtinNames modelData ++ executableList modelData
    rules = mCompletionRules modelData
    
    matchIndex' = matchIndex <&> ((length (T.words inp)-1)-)
    (matchIndex, cursorIndex) = extractIndexes inp (cursorLocation modelData)
    fileMatches exec = ((&&) <$> doesDirectoryExist d <*> (getPermissions d <&> readable)) >>= 
        bool (pure []) (getDirectoryContents d <&> filter (T.isPrefixOf exec) . (bool id (T.pack . (d</>) . T.unpack) (T.pack d `T.isPrefixOf` exec) <$>) . fmap T.pack)
      where 
        d = takeDirectory (T.unpack exec)

extractIndexes text cursor = case f cursor (T.reverse text) of
      Nothing -> (Nothing, Nothing)
      Just (a,b) -> bool (Nothing, Nothing) (Just a, Just b) (a<length ( T.words text))
      where
        f :: Int -> T.Text -> Maybe (Int, Int)
        f cursor text
          | T.null text = Just (0, 0)
          | T.head text == ' ' = Nothing
          | and [T.length (head $ T.words text) > cursor, not . isSpace $ T.head text] = Just (0, cursor)
          | otherwise =  (T.stripPrefix (head $ T.words text) text) 
            >>= (\x -> (bool (f (cursor - T.length (head $ T.words text) - countLeadingWhitespace x) (T.strip x)) Nothing (countLeadingWhitespace x > cursor - T.length (head $ T.words text))))
            <&> first (1+)

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

languageHook :: AutocompleteModelData -> IO ()
languageHook modelData = unless (T.null input) $
  bool
    afterCursor -- redraw only right, as you're within a word so now quirky stuff
    whole       -- redraw everything, as you're jumping between words.
    (isNothing matchIndex)
  where
    input = modelInput modelData 
    cursor = cursorLocation modelData
    cursor' = T.length input - cursor
    cscheme = aColorScheme modelData
    model = modelOutput modelData
    executables = executableList modelData ++ builtinNames modelData
    rules = mCompletionRules modelData

    (matchIndex, cursorIndex) = extractIndexes input cursor

    afterCursor, whole :: IO ()

    curWord = matchIndex <&> (reverse (T.words input)!!)

    {-whitespace = segmentWhiteSpace input
    whitespace' = case matchIndex of 
      Just x  -> reverse $ take x $ reverse $ segmentWhiteSpace input
      Nothing -> []-}
    whole = resetCursor input cursor >> eraseRight >> (wholeAnsi >>= putStrf) >> retrieveCursor
    afterCursor = toCurWordStart >> eraseRight >> (rightAnsi >>= \x -> putStrf x) >> retrieveCursor >> retrievePrediction

    -- TODO: add `langAsAnsi` contextual modes {Executable, Argument} -- not really

    wholeAnsi = langAsAnsi rules input cscheme cursor executables

    -- fault lies here
    rightAnsi = case runParser parseExpr input of
      Nothing -> pure ""
      Just (_, n) -> case n of
        ProgramCall e a -> bool
        -- ???
          (case findArg a (cursor' - T.length (complexToRawText e)) of
            Just (i,_) -> shadowText cscheme >>= \shadow -> isValidArgument rules (exec':take (i+1) (fmap (\(c, _) -> complexToRawText' c) a))
                          >>= bool (pure $ sp1<>"\ESC[4m"<>w<>"\ESC[0m"<>sp2<>asciiColor shadow<>pred'<>"\ESC[0m") (pure $ sp1<>w<>sp2<>asciiColor shadow<>pred'<>"\ESC[0m")
              where
                pred' = fromMaybe "" pred
                (w,(sp1,sp2)) = first complexToRawText' (a!!i)
            Nothing -> undefined
          )
          ( executable' >>= (\x -> shadowText cscheme <&> (\st -> x <> asciiColor st <> prediction'' <> "\ESC[0m" <> executableSpace' <> args')) )
          (cursor' <= elen)
          where
            elen = T.length (complexToRawText' $ fst e)
            exec' = complexToRawText' $ fst e
            executable' = formatted exec'
            executableSpace' = (snd . snd) e
            prediction'' = fromMaybe "" prediction'
            prediction' = case fst model of
              (x:_) -> curWord >>= (`T.stripPrefix` x)
              [] -> Nothing
            args' = T.concat (fmap complexToRawText a)
            --executableSpace'
        _ -> undefined
      where
        formattedWord word e = (if e || T.null word then successColor cscheme else errorColor cscheme) <&> (<>word<>"\ESC[0m") . asciiColor
        formatted exec = findExecutable (T.unpack exec) >>= formattedWord exec . (|| exec `elem` executables) . isJust
    --rightAnsi = shadowText cscheme >>= (\stext -> wholeAnsi <&> addPrediction stext . (T.reverse . T.take (T.length right) . T.reverse))

    {-addPrediction stext x = case T.words x of
      (w:_) -> case pred of
        Just p -> fst splt <> asciiColor stext <> p <> "\ESC[0m" <> snd splt where splt = T.splitAt (T.length w) x
        Nothing -> x
      _ -> x-}
    
    addPrediction stext (x:xs) = case pred of
      Just y  -> (x<>asciiColor stext<>y<>"\ESC[0m"):xs
      Nothing -> x:xs

    --displayCurrent = for_ curWord (\x -> langAsAnsi x whitespace' cscheme cursor executables >>= putStrf)

    pred = case fst model of 
      (x:_) -> curWord >>= (`T.stripPrefix` x)
      [] -> Nothing
    --displayPrediction = putStrf pred
    (displayPrediction, retrievePrediction) = case pred of 
      Just p -> (shadowText cscheme >>= putStrf . (<>p<>"\ESC[0m") . asciiColor, when (T.length p>0) $ moveCursor DLeft $ T.length p)
      Nothing -> (pure (), pure ())
    retrieveCursor = when (r > 0) $ moveCursor DLeft r where r = cursor

    right = T.reverse $ T.take (cursor + T.length curWordLeft) $ T.reverse input
    (curWordLeft, curWordRight) = case (curWord, cursorIndex) of
      (Just x, Just y) -> (T.take (T.length x - y) x, T.reverse $ T.take y $ T.reverse x)
      _ -> ("", "")

    toCurWordStart = bool (pure ()) (moveCursor DLeft (T.length curWordLeft)) (T.length curWordLeft > 0)

resetCursor :: T.Text -> Int -> IO ()
resetCursor t i = when (T.length t > i) $ moveCursor DLeft (T.length t - i)

eraseRight :: IO ()
eraseRight = putStrf "\ESC[0K"

defaultHook :: AutocompleteModelData -> IO ()
defaultHook modelData = unless (T.null t) $
    bool
      ({-when (i >= T.length t - T.length fstWord) re-add I thibk-}resetCursor t i >> displayExecutable >> moveToCursor >> toEOW >> displayPrediction >> displayRight >> retrieveRight >> retrievePrediction >> fromEOW)
      (resetCursor t i>> eraseRight >> displayExecutable >> displayLeft >> displayCurrent >> displayPrediction >> displayRight >> retrieveCursor >> retrievePrediction)
      (isNothing matchIndex)
  where
    t = modelInput modelData
    i = cursorLocation modelData
    c = aColorScheme modelData
    m = modelOutput modelData
    fstWord = head $ T.words t

    {-toEOW = when (cursorIndex > 0) $ 
        moveCursor DRight cursorIndex
    fromEOW = when (cursorIndex > 0) $
        moveCursor DLeft cursorIndex-}
    (toEOW, fromEOW) = case cursorIndex of
      Just x -> (when (x > 0) $ moveCursor DRight x,
                 when (x>0) $ moveCursor DLeft x)
      Nothing -> (pure (), pure ())
    (matchIndex, cursorIndex) = extractIndexes t i

    curWord = matchIndex <&> (reverse (T.words t)!!) 

    displayCurrent = for_ curWord putStrf

    pred = case fst m of 
      (x:_) -> curWord >>= (`T.stripPrefix` x)
      [] -> Nothing
    (displayPrediction, retrievePrediction) = case pred of 
      Just p -> (shadowText c >>= putStrf . (<>p<>"\ESC[0m") . asciiColor, when (T.length p>0) $ moveCursor DLeft $ T.length p)
      Nothing -> (pure (), pure ())

    (curWordLeft, curWordRight) = case (curWord, cursorIndex) of
      (Just x, Just y) -> (T.take (T.length x - y) x, T.reverse $ T.take y $ T.reverse x)
      _ -> ("", "")
    displayRest = for_ (T.stripPrefix fstWord $ T.stripStart t) putStrf

    left = bool "" (unwrap $ T.stripPrefix fstWord $ T.take (T.length t - i - T.length curWordLeft) t) (T.length t > i)
    displayLeft = putStrf left

    right = bool r (unwrap $ T.stripPrefix fstWord r) (fstWord `T.isPrefixOf` r)  where r = T.reverse $ T.take (i - T.length curWordRight) $ T.reverse t
    displayRight = putStrf right
    retrieveRight = when (T.length right>0) $ moveCursor DLeft $ T.length right
    rest = fromMaybe "" $ T.stripPrefix fstWord $ T.stripStart t
    retrieveCursor = when (r > 0) $ moveCursor DLeft r where r = i


    formattedWord e = (if e || T.null fstWord then successColor c else errorColor c) <&> (<>fstWord<>"\ESC[0m") . asciiColor
    displayExecutable = (findExecutable (T.unpack fstWord) >>= formattedWord . isJust) >>= putStrf

    moveToCursor = bool (when (T.length rest > i) $ moveCursor DRight (T.length rest - i)) (moveCursor DLeft (i - T.length rest)) (i > T.length rest) 


data AutocompleteConfig = AutocompleteConfig {
    model     :: AutocompleteModel
  , redrawHook:: AutocompleteModelData -> IO ()
  }

instance Def AutocompleteConfig where
  def = AutocompleteConfig {
    model = defaultModel
  , redrawHook = defaultHook
  }

-- horrible sin
unwrap (Just x) = x

countLeadingWhitespace :: T.Text -> Int
countLeadingWhitespace t
      | T.null t  = 0
      | not (isSpace $ T.head t) = 0
      | isSpace (T.head t) = countLeadingWhitespace (T.tail t) + 1
      | otherwise = 0


segmentWhiteSpace :: T.Text -> [T.Text]
segmentWhiteSpace t
      | T.null t = []
      | isSpace (T.head t) = (T.pack [' ' | _<-[1..T.length t - T.length t2]]):segmentWhiteSpace t2 
      | otherwise = segmentWhiteSpace $ T.dropWhile (/=' ') t
      where t2 = T.dropWhile (==' ') t


-- ansi stuff
langAsAnsi :: [CompletionRule] -> T.Text -> ColorScheme -> Int -> [T.Text] -> IO T.Text
langAsAnsi rules t colorScheme cursor executables = case runParser parseExpr t of
  Just (r, node) -> case node of
    (ProgramCall e a) -> do
      let exec = complexToRawText' $ fst e
      let (a',b') = snd e
      --zip wws args <- this is good I think
      --aargs <- 
      (<>) <$> (formatted exec <&> (a'<>) . (<>b')) <*> formatArgs [exec] a colorScheme
      where
        formattedWord word e = (if e || T.null word then successColor colorScheme else errorColor colorScheme) <&> (<>word<>"\ESC[0m") . asciiColor
        formatted exec = findExecutable (T.unpack exec) >>= formattedWord exec . (|| exec `elem` executables) . isJust

        -- work on this (!!). It breaks on  `whole`. Figure out why.
        formatArgs :: [T.Text] -> [StringComplex] -> ColorScheme -> IO T.Text
        --formatArgs executable arguments whitespace cscheme = (<>) 
        formatArgs t' ((a', (lft,rgt)):as) cs = (<>) <$> n <*> (nt >>= \x -> formatArgs x as cs)
          where
            n :: IO T.Text
            nt :: IO [T.Text]
            n = isValidArgument rules (t'++[complexToRawText' a']) <&> (lft<>) . (<>(complexToRawText' a' <>"\ESC[0m" <> rgt)) . bool "\ESC[4m" ""
            nt = n <&> (t'++) . singleton
        formatArgs _ [] _ = pure ""
        formatArgs _ [] _ = pure ""
        formatArgs _ x _ = pure ""
        --formatArgs _ _  _ _ = pure ""
    {-(And a b) -> do
      a_u <- nodeAsText a
      b_u <- langAsAnsi b whitespace' colorScheme cursor

      pure $ a_u <> w1 <> "&&" <> w2 <> b_u
      where
        whitespace' = undefined
        w1 = undefined
        w2 = undefined-}
    _ -> undefined
  Nothing -> (\x y z -> x<>y<>z) <$> (err <&> asciiColor) <*> pure t <*> pure "\ESC[0m"
    where err = errorColor colorScheme
