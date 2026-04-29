{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module FokShell.Module.Preprocessor.StringPreprocessors where
import FokShell.Module.Preprocessor
import Language.Parser

import Data.Text qualified as T
import Data.Map qualified as M
import Control.Arrow (Arrow(second))
import Data.Maybe (fromJust)
import Data.Bool (bool)
import Debug.Trace (trace)
import System.Environment.Blank (getEnv)
import Data.Functor ((<&>))
import Control.Applicative
combineStringPreprocessors :: [Preprocessor] -> Preprocessor
combineStringPreprocessors pp n@(NodeString _ True) = connectPreprocessors pp n
combineStringPreprocessors _  n@(NodeString _ False) = pure n
combineStringPreprocessors pp n@(Path _) = connectPreprocessors pp n
combineStringPreprocessors pp (Table t) = Table . M.fromList <$> mapM (\(x,y) -> do
      y' <- combineStringPreprocessors pp y
      pure (x,y')
    ) (M.toList t)
combineStringPreprocessors pp (Array a) = Array <$> mapM (combineStringPreprocessors pp) a
combineStringPreprocessors pp (ProcessCall name args) = ProcessCall <$> combineStringPreprocessors pp name <*> mapM (combineStringPreprocessors pp) args
combineStringPreprocessors pp (And left right) = And <$> combineStringPreprocessors pp left <*> combineStringPreprocessors pp right
combineStringPreprocessors pp (Sequence left right) = Sequence <$> combineStringPreprocessors pp left <*> combineStringPreprocessors pp right
combineStringPreprocessors pp (Or left right) = Or <$> combineStringPreprocessors pp left <*> combineStringPreprocessors pp right
combineStringPreprocessors pp (Pipe ps left right) = Pipe ps <$> combineStringPreprocessors pp left <*> combineStringPreprocessors pp right

substituter :: T.Text -> IO T.Text -> Int -> Preprocessor
substituter pat with times (NodeString s True) = do
  with' <- with
  pure $ (`NodeString` True) $ replaceN times pat with' s
substituter _ _ _ _ = undefined

replaceN :: Int -> T.Text -> T.Text -> T.Text -> T.Text
replaceN 0 _ _ t = t
replaceN x pat with input = bool (error "negative number of replaces in replaceN") (left <> right) (x>0)
  where
    (left', right') = T.breakOn pat input
    (left, right) = second (replaceRight . T.stripPrefix pat) (left', right')
    replaceRight (Just y) = with <> replaceN (x-1) pat with y
    replaceRight Nothing = right'


envVarPreprocessor :: Preprocessor
envVarPreprocessor (NodeString s True) = case runParser (many substringParser) s of
    Just (leftover, xs) -> (`NodeString` True) . (<>leftover) . T.concat <$> sequence xs
    Nothing -> pure $ NodeString s True
envVarPreprocessor _ = undefined
substringParser :: Parser (IO T.Text)
substringParser  = envvarParser <|> (pure <$> basicParser)

envvarParser :: Parser (IO T.Text)
envvarParser = do
  a <- charP '$' *> (clauseParser <|> limitedParser)
  pure $ T.pack <$> (getEnv (T.unpack a) <&> \case
          Just x -> x
          Nothing-> trace ("non-existent env variable accessed: "<>T.unpack a) "")

basicParser :: Parser T.Text
basicParser = spanPForce (`notElem` (" ${}" :: String))

limitedParser :: Parser T.Text
limitedParser = spanPForce (`notElem` (" ${}/" :: String))

clauseParser :: Parser T.Text
clauseParser = charP '{' *> basicParser <* charP '}'
