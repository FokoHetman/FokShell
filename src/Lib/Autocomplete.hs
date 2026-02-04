{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Lib.Autocomplete where

import qualified Data.Text as T
import Lib.Primitive
import Lib.Format
import Lib.ColorScheme
import Data.Maybe (isJust, fromMaybe, isNothing, fromJust)
import Data.Functor
import System.Directory (findExecutable, getDirectoryContents, getPermissions, Permissions (readable), doesDirectoryExist)
import Control.Monad (when, unless)

import System.FilePath.Posix ((</>), takeDirectory)
import Data.Char (isSpace)
import Data.Bool (bool)
import Control.Arrow (Arrow(first))
import Data.List (singleton)

import Language.Parser
import System.Environment (getEnv, lookupEnv, getEnvironment)
import Debug.Trace (traceShow)
import Data.Bifunctor (Bifunctor(bimap, second))


type AutocompleteModel = AutocompleteModelData -> IO ([T.Text], [T.Text])
{- todo: make input a Record, consolidate last 2 args into passing Caches -}
{- what? -}

findArg' :: (Int -> Int) -> [StringComplex] -> Int -> Maybe (Int, (Int,T.Text))
findArg' f ((str, (a, b)):ts) i = bool (first (+1) <$> findArg' f ts (f $ i - {-?-} T.length a - T.length t - T.length b)) (t' <&> \tt -> (0, (i - T.length a,tt))) (T.length t >= i && i >= 0)
  where 
    t = a<>complexToRawText' str
    t' = case str of
      Basic s -> Just s
      Variant s -> case snd <$> findArg' ((\x -> x-1) . f) s (i - T.length a - 1) of
        Just x -> Just $ snd x
        Nothing -> Nothing
      Combination _ -> error "idk bruv"
      EnvVar _ -> undefined
      _ -> error "?"
findArg' _ [] _ = Nothing

findArg = findArg' id

findArgVariant vs i = findArg' (\x -> x-1) vs (i-1)

languageModel :: AutocompleteModel
languageModel modelData = case parsed of
    Just (_, n)  -> case n of
      ProgramCall exec args -> bool
        {-matching args-}
        (argMatches <&> (,[]))
        {-matching executable-}
        (executableMatches (cursor - T.length left) (fst exec) <&> (,[]) )
        (T.length exec' + T.length left >= cursor)
        where
          (exe, (left,_right)) = exec
          exec' = complexToRawText' exe
          exec'' = complexToRawText exec
          executableMatches i e = case e of
            Basic b     -> pure $ filter (T.isPrefixOf b) executables
            Variant vs  -> pure $ case findArgVariant vs (i-1) of
              Just (_,(_,t))-> filter (T.isPrefixOf t) executables
              _         -> []
            Combination ts -> pure $ case findArg ts i of
              Just (_,(_,t))-> filter (T.isPrefixOf t) executables
              _         -> []
            EnvVar a    -> lookupEnv (T.unpack a) <&> \case 
              Just x -> ((`filter` executables) . T.isPrefixOf . T.pack) x
              Nothing -> []
              
          rule = lookupRule exec' rules
          curArg = findArg args (cursor - T.length exec'')
          argMatches = case rule of
            Just r -> case curArg of
              Nothing -> pure []
              Just (x,_)  -> fmap (\(CompRule e _) -> e) <$> nestNTimes r (fmap complexToRawText args) x
            Nothing -> case curArg of 
              Just (_,(_,arg)) -> fileMatches arg
              Nothing -> pure []
      _ -> error "undefined behavior"
    Nothing  -> pure ([], [])
  where
    inp = modelInput modelData
    cursor = T.length inp - cursorLocation modelData
    parsed = runParser parseExpr inp
    executables = builtinNames modelData ++ executableList modelData
    rules = mCompletionRules modelData
    fileMatches exec = ((&&) <$> doesDirectoryExist d <*> (getPermissions d <&> readable)) >>= 
        bool (pure []) (getDirectoryContents d <&> filter (T.isPrefixOf exec) . (bool id (T.pack . (d</>) . T.unpack) (T.pack d `T.isPrefixOf` exec) <$>) . fmap T.pack)
      where 
        d = takeDirectory (T.unpack exec)


track :: Node -> Int -> StringComplex'
track (ProgramCall (e,_) a) i = bool
  (fst $ last $ take (i+1) a)
  e
  (i==0)
track _ _ = undefined

trackword :: StringComplex' -> Int -> T.Text
trackword (Basic b) c = bool "" b (T.length b >= c)
trackword (EnvVar e) c = bool "" e (T.length e >= c)
trackword (Variant vs) c = case findArg vs c of
  Just (_,(_,t)) -> t
  Nothing -> ""
trackword (Combination vs) c = error "idk yet ngl"


extract :: Node -> Int -> Maybe (Int, Int)
extract (ProgramCall (s', (a,b)) as) c = bool
  (bool
    (bool
      Nothing
      (Just (0, c - T.length a))
      (T.length a + T.length s >= c)
    )
    (bimap (+1) fst <$> findArg as (c - T.length s - T.length a - T.length b))
    (T.length a + T.length s + T.length b < c)
  )
  Nothing
  (T.length a > c)
  
  where
    s = complexToRawText' s'
extract _ _ = undefined

extract' :: T.Text -> Int -> (Maybe Int, Maybe Int)
extract' input cursor = case runParser parseExpr input of
      Just (_,n) -> case extract n cursor of
        Just (x,y)  -> (Just x, Just y)
        Nothing     -> (Nothing, Nothing)
      Nothing    -> (Nothing, Nothing)

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

-- (complex, (charindex, ~wordindex))
findComplex :: [StringComplex] -> Int -> Int -> Maybe (StringComplex,(Int,Int))
findComplex (s:ss) cursor padding = bool
  Nothing
  (bool
    (second (second (+1)) <$> findComplex ss (cursor - T.length rawS - padding) padding)
    (Just (s, (cursor,0)))
    (T.length rawS >= cursor)
  )
  (cursor>=0)
  where
  rawS = complexToRawText s
findComplex [] _ _ = Nothing


currentComplex :: Node -> Int -> Maybe (StringComplex,(Int,Int))
currentComplex (ProgramCall (exec,(a,b)) args) c = bool
  (second (second (+1)) <$> findComplex args (c - T.length rawExec) 0)
  (
  bool 
  Nothing
  (Just ((exec,(a,b)),(c,0)))
  (T.length a <= c && T.length rawExec' + T.length a >= c)
  )
  (T.length rawExec >= c)
  where
  rawExec = complexToRawText (exec,(a,b))
  rawExec' = complexToRawText' exec

currentNode :: Node -> Int -> Maybe (Node, Int)
currentNode (ProgramCall e a) c = Just (ProgramCall e a, c)
currentNode _ _ = error "todo"

clear, clearRight :: T.Text
clear = "\ESC[0m"
clearRight = "\ESC[0K"

-- TODO: make it handle spaces properly (returning to words breaks cursor positioning)

-- kurwa mać japierdole nie wiem chyba trzeba matchować node'y i wyświetlać po kolei japierdole 
-- ^ he's right btw
-- TODO:
-- fix args formatting like execs and other edge cases
-- fix arg predictions not displaying

at :: Int -> (a -> a) -> [a] -> [a]
at _ _ [] = []
at 0 f (x:xs) = f x:xs
at i f (x:xs) = x: at (i-1) f xs

languageHook :: AutocompleteModelData -> IO ()
languageHook mData = unless (T.null input || Just True/=(T.null <$> rest)) $
  case masterNode of
    Just (ProgramCall (e,(a,b)) args) -> case currentComplex (fromJust masterNode) i of
      Just (complex,(char,index)) -> when (char>0) (moveCursor DLeft char) >> case complex of
        (Basic basic', (a,b)) -> do 
          args'' <- args'
          fmt <- wordFormat basic' index
          s <- shadowText cscheme
          putStrf (a<>fmt<>basic'<>asciiColor s<>predi<>clear<>b<>args'') 
          putStrf clearRight
          let d = T.length basic' + T.length a+ T.length b  - char + T.length predi + T.length rawargs
          when (d>0) (moveCursor DLeft d)
          where
            predi = prediction' basic'
        (Variant vs, (a',b')) -> do 
          args'' <- args' 
          s <- shadowText cscheme <&> asciiColor
          vargs <- mapM (\x -> let (x',(a,b)) = x in wordFormat (complexToRawText' x') index <&> (a<>) . (<>complexToRawText' x'<>clear<>b)) vs
          (putStrf . ((a'<>"{")<>) . (<>("}"<>b'<>args'')) . T.intercalate "," . at argindex (<>s<>predi<>clear)) vargs
          putStrf clearRight
          moveCursor DLeft (cursor + T.length predi)
          where
            arg = findArgVariant vs (cursor' - bool (T.length (complexToRawText (e,(a,b)))) 0 (index==0) - T.length a')
            (argindex,predi) = case arg of
              Just (argi,(_,t)) -> (argi, prediction' t)
              Nothing -> (0,"")
        (EnvVar e, (a,b)) -> args' >>= \args'' -> (getEnvironment >>= wordFormat' e index . fmap (T.pack . fst))
          >>= putStrf . (((a<>"$")<>e)<>) . (<>b<>args'')
        _ -> error "no impl"
        where
        rawargs = T.concat (fmap complexToRawText (reverse $ take (length args - index) $ reverse args))
        argst = fmap complexToRawText args
        args' = formatArgs (bool (complexToRawText' e:take (index-1) argst) [complexToRawText' e] (index==0)) (reverse $ take (length args - index) $ reverse argst) (1+index)
        formatArgs prev (n:ns) i = (\x y -> x<>n<>y) <$> wordFormat' n i [] <*> formatArgs (prev++[n]) ns (i+1)
        formatArgs _ [] _ = pure ""
      Nothing -> whole
      where
      n = fromJust masterNode
      i = cursor'
      wordFormat x y = wordFormat' x y executables
      wordFormat' :: T.Text -> Int -> [T.Text] -> IO T.Text
      wordFormat' word index argvs = bool
        (case n of 
          -- todo: stop hard coding \ESC[4m
          ProgramCall e a -> isValidArgument rules (complexToRawText' (fst e):take (i+1) (fmap (\(c, _) -> complexToRawText' c) a)) >>= bool ((<>"\ESC[4m") . asciiColor <$> textColor cscheme) (asciiColor <$> textColor cscheme)
          _ -> error "idk if I should actually handle this edge case"
        )
        (asciiColor <$> bool (errorColor cscheme) (successColor cscheme) (word `elem` argvs))
        (index == 0)
      -- todo: stop hard coding the "fst" and make a configurable function taking history and execs instead
      prediction' x = fromMaybe "" (prediction x)
      prediction :: T.Text -> Maybe T.Text
      prediction x = case n of 
        ProgramCall _ _ -> case fst model of 
          (s:_) -> x `T.stripPrefix` s
          [] -> Nothing
        _ -> Nothing
    _ -> whole
    where
    input = modelInput mData
    cursor = cursorLocation mData
    cursor' = T.length input - cursor
    executables = executableList mData ++ builtinNames mData
    rules = mCompletionRules mData
    cscheme = aColorScheme mData
    model = modelOutput mData

    whole = resetCursor input cursor >> eraseRight >> (langAsAnsi rules input cscheme cursor executables >>= putStrf) >> when (cursor > 0) (moveCursor DLeft cursor)
    r = runParser parseExpr input
    rest = fst <$> r
    masterNode = snd <$> r
{-languageHook mData = unless (T.null input) $
  case masterNode >>= (`currentNode` cursor') of
    Just (n, i) -> case currentComplex n i of
      -- if currently a node is hovered on, redraw it and anything right of it
      Just (complex,(char,index)) -> when (char>0) (moveCursor DLeft char) >> case complex of
        (Basic basic, (a,b)) -> wordFormat basic index >>= \fmt -> shadowText cscheme >>= \s -> putStrf (a<>fmt<>basic<>asciiColor s<>prediction'<>clear<>b)
            >> when (cursor + T.length prediction'>0) (moveCursor DLeft (cursor + T.length prediction'))
        (Variant vs, (a',b')) -> (mapM (\x -> let (x',(a,b)) = x in wordFormat (complexToRawText' x') index <&> (a<>) . (<>complexToRawText' x'<>clear<>b)) vs
            >>= putStrf . ((a'<>"{")<>) . (<>("}"<>b')) . T.intercalate ",")
          >> moveCursor DLeft (cursor + T.length prediction')
        (EnvVar e, (a,b)) -> (getEnvironment >>= wordFormat' e index . fmap (T.pack . fst))
          >>= putStrf . (((a<>"$")<>e)<>) . (<>b)
        _ -> error "no impl"
      Nothing -> whole
      where
      wordFormat x y = wordFormat' x y executables
      wordFormat' :: T.Text -> Int -> [T.Text] -> IO T.Text
      wordFormat' word index argvs = bool
        (case n of 
          -- todo: stop hard coding \ESC[4m
          ProgramCall e a -> isValidArgument rules (complexToRawText' (fst e):take (i+1) (fmap (\(c, _) -> complexToRawText' c) a)) >>= bool ((<>"\ESC[4m") . asciiColor <$> textColor cscheme) (asciiColor <$> textColor cscheme)
          _ -> error "idk if I should actually handle this edge case"
        )
        (asciiColor <$> bool (errorColor cscheme) (successColor cscheme) (word `elem` argvs))
        (index == 0)
      -- todo: stop hard coding the "fst" and make a configurable function taking history and execs instead
      prediction' = fromMaybe "" prediction
      prediction :: Maybe T.Text
      prediction = case n of 
        ProgramCall e _ -> case fst model of 
          (x:_) -> complexToRawText' (fst e) `T.stripPrefix` x
          [] -> Nothing
        _ -> Nothing
    -- upon Nothing, redraw entire prompt
    Nothing -> whole
  where
    input = modelInput mData
    cursor = cursorLocation mData
    cursor' = T.length input - cursor
    executables = executableList mData ++ builtinNames mData
    rules = mCompletionRules mData
    cscheme = aColorScheme mData
    model = modelOutput mData

    whole = resetCursor input cursor >> eraseRight >> (langAsAnsi rules input cscheme cursor executables >>= putStrf) >> when (cursor > 0) (moveCursor DLeft cursor)

    masterNode = snd <$> runParser parseExpr input
-}

{-languageHook :: AutocompleteModelData -> IO ()
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

    (matchIndex, cursorIndex) = extract' input cursor'

    afterCursor, whole :: IO ()

    (curWordIndex, curWord) = case runParser parseExpr input of
      Just (_, n) -> case extract n cursor' of
        Just (i,c) -> (Just i, Just $ trackword (track n i) c)
        Nothing -> (Nothing, Nothing)
      Nothing -> (Nothing, Nothing)

    whole = wholeAnsi >>= whole'
    whole' ansi = resetCursor input cursor >> eraseRight >> putStrf ansi >> retrieveCursor
    afterCursor = toCurWordStart >> eraseRight >> rightAnsi >> retrieveCursor >> retrievePrediction

    wholeAnsi = langAsAnsi rules input cscheme cursor executables

    rightAnsi :: IO ()
    rightAnsi = case runParser parseExpr input of
      Nothing -> {-revert toCurWordStart-} when (T.length curWordLeft>0) (moveCursor DRight (T.length curWordLeft))
        >> (((\x y z -> x<>y<>z) <$> (err <&> asciiColor) <*> pure input <*> pure "\ESC[0m") >>= whole')
        where err = errorColor cscheme
      Just (_, n) -> case n of
        ProgramCall e a -> case curWord of 
          Just curword -> bool
            (case findArg a (cursor' - T.length (complexToRawText e)) of
              Just (i,_) -> shadowText cscheme >>= \shadow -> isValidArgument rules (curword:take (i+1) (fmap (\(c, _) -> complexToRawText' c) a))
                    >>= bool (putStrf $ sp1<>"\ESC[4m"<>w<>"\ESC[0m"<>sp2<>asciiColor shadow<>pred'<>"\ESC[0m") (putStrf $ sp1<>w<>sp2<>asciiColor shadow<>pred'<>"\ESC[0m")
                where
                  pred' = fromMaybe "" prediction
                  (w,(sp1,sp2)) = first complexToRawText' (a!!i)
              Nothing -> when (T.length curWordLeft > 0) (moveCursor DRight (T.length curWordLeft)) >> whole
            )
            (case fst e of
              Variant vs -> when (cursor' > 0) (case findArg' (\x -> x-1) vs (cursor'-1) of
                Just (i,(c,t)) ->
                  when (c>0) (moveCursor DLeft c) >> (shadowText cscheme >>= putStrf . (\st -> t <> asciiColor st <> predictionV' <> "\ESC[0m" <> space <> rest <> "}" <> executableSpace' <> args')) >> when (c>0) (moveCursor DRight c)
                  where
                  predictionV' = fromMaybe "" predictionV
                  predictionV = case fst model of
                    (x:_) -> t `T.stripPrefix` x
                    [] -> Nothing
                  space = (snd . snd) $ vs!!i
                  rest = bool "" "," (length vs - i - 1 > 0) <> T.intercalate "," (complexToRawText <$> take (length vs - i - 1) (reverse vs))
                Nothing -> pure ())
              _ -> executable' >>= (\x -> shadowText cscheme >>= putStrf . (\st -> x <> asciiColor st <> prediction'' <> "\ESC[0m" <> executableSpace' <> args'))
            )
            (cursor' <= elen)
            where
              elen = T.length (complexToRawText' $ fst e)
              executable' = formatted curword
              executableSpace' = (snd . snd) e
              prediction'' = fromMaybe "" prediction'
              prediction' = case fst model of
                (x:_) -> curword `T.stripPrefix` x
                [] -> Nothing
              args' = T.concat (fmap complexToRawText a)
          Nothing -> pure ()
          
        _ -> undefined
      where
        formattedWord word e = (if e || T.null word then successColor cscheme else errorColor cscheme) <&> (<>word<>"\ESC[0m") . asciiColor
        formatted exec = findExecutable (T.unpack exec) >>= formattedWord exec . (|| exec `elem` executables) . isJust

    prediction = case fst model of 
      (x:_) -> curWord >>= (`T.stripPrefix` x)
      [] -> Nothing
    (_displayPrediction, retrievePrediction) = case prediction of 
      Just p -> (shadowText cscheme >>= putStrf . (<>p<>"\ESC[0m") . asciiColor, when (T.length p>0) $ moveCursor DLeft $ T.length p)
      Nothing -> (pure (), pure ())
    retrieveCursor = when (r > 0) $ moveCursor DLeft r where r = cursor

    _right = T.reverse $ T.take (cursor + T.length curWordLeft) $ T.reverse input
    (curWordLeft, _curWordRight) = case (curWord, cursorIndex) of
      (Just x, Just y) -> (T.take y x, T.reverse $ T.take (T.length x - y) $ T.reverse x)
      _ -> ("", "")

    toCurWordStart = bool (pure ()) (moveCursor DLeft (T.length curWordLeft)) (T.length curWordLeft > 0)
-}
resetCursor :: T.Text -> Int -> IO ()
resetCursor t i = when (T.length t > i) $ moveCursor DLeft (T.length t - i)

eraseRight :: IO ()
eraseRight = putStrf "\ESC[0K"


data AutocompleteConfig = AutocompleteConfig {
    model     :: AutocompleteModel
  , redrawHook:: AutocompleteModelData -> IO ()
  }

instance Def AutocompleteConfig where
  def = AutocompleteConfig {
    model = languageModel
  , redrawHook = languageHook
  }

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
  Just (_r, node) -> case node of
    (ProgramCall e a) -> do
      --zip wws args <- this is good I think
      --aargs <- 
      (<>) <$> es <*> formatArgs' (exec $ fst e) [] a colorScheme postcursor
      where
        exec :: StringComplex' -> IO T.Text
        exec = \case
          Basic tt -> pure tt
          EnvVar tt -> lookupEnv (T.unpack tt) <&> \case
            Just x -> T.pack x
            Nothing -> tt
          Variant (v:vs) -> exec $ fst v
          Variant [] -> pure ""
          Combination xs -> T.concat <$> mapM (exec . fst) xs

        es' :: StringComplex -> IO T.Text
        es' (e', (lft',rgt')) = (case e' of
          Basic tt -> formatted tt
          EnvVar tt -> lookupEnv (T.unpack tt) >>= formattedWord ("$"<>tt) . isJust
          Variant vs -> ("{"<>) . (<>"}") . T.intercalate ","  <$> mapM es' vs
          Combination xs -> T.concat <$> mapM es' xs) <&> (lft'<>) . (<>rgt')
        rawParse = complexToRawText e
        es = es' e
        postcursor = cursor - T.length rawParse
        formattedWord word isexec = (if isexec || T.null word then successColor colorScheme else errorColor colorScheme) <&> (<>word<>"\ESC[0m") . asciiColor
        formatted exec' = findExecutable (T.unpack exec') >>= formattedWord exec' . (|| exec' `elem` executables) . isJust

        -- work on this (!!). It breaks on  `whole`. Figure out why.
        formatArgs :: [T.Text] -> [StringComplex] -> ColorScheme -> Int -> IO T.Text
        --formatArgs executable arguments whitespace cscheme = (<>) 
        formatArgs t' ((a', (lft,rgt)):as) cs cursor' = (<>) <$> n <*> (nt >>= \x -> formatArgs x as cs ncursor)
          where
            n :: IO T.Text
            nt :: IO [T.Text]
            (current, ncursor) = case a' of
              Basic x -> (x, cursor' - T.length x - T.length lft - T.length rgt)
              Variant vs -> case findArg' (\x -> x-1) vs cursor' of
                Just (_,(c,x)) -> (x,c)
                Nothing -> ("",cursor')
              _ -> undefined
            n = textColor cs >>= \tc -> isValidArgument rules (t'++[current]) <&> (lft<>) . (<>(asciiColor tc<>complexToRawText' a' <>"\ESC[0m" <> rgt)) . bool "\ESC[4m" ""
            nt = n <&> (t'++) . singleton
        formatArgs _ [] _ _ = pure ""
        formatArgs' text prev args cs c = text >>= \x -> formatArgs (x:prev) args cs c
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
