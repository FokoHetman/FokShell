{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Preprocessor.StringPreprocessors where
import FokShell.Module.Preprocessor
import Language.Parser

import Data.Text qualified as T
import Data.Map qualified as M
import Control.Arrow (Arrow(second))
import Data.Maybe (fromJust)
import Data.Bool (bool)
import Debug.Trace (traceShow)

combineStringPreprocessors :: [Preprocessor] -> Preprocessor
combineStringPreprocessors pp n@(NodeString _) = connectPreprocessors pp n
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
substituter pat with times (NodeString s) = do
  with' <- with
  pure $ NodeString $ replaceN times pat with' s
substituter _ _ _ _ = undefined

replaceN :: Int -> T.Text -> T.Text -> T.Text -> T.Text
replaceN 0 _ _ t = t
replaceN x pat with input = bool (error "negative number of replaces") (left <> right) (x>0)
  where
    (left', right') = T.breakOn pat input
    (left, right) = second (replaceRight . T.stripPrefix pat) (left', right')
    replaceRight (Just y) = with <> replaceN (x-1) pat with y
    replaceRight Nothing = right'
