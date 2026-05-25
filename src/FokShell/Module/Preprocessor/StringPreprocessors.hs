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
import Data.Data (Proxy(Proxy))
import FokShell.Types (ShellConfig)

substituter :: T.Text -> (ShellConfig -> IO T.Text) -> Int -> Preprocessor
substituter pat with times p n = do
  with' <- with p
  pure . modifyNode n $ \node -> case withProxyNode (Proxy @Primitive) node of
    Just (NodeString _ SingleQuote) -> node
    Just (NodeString t q) -> Node $ NodeString (replaceN times pat with' t) q
    _ -> node

substituteprefix :: T.Text -> (ShellConfig -> IO T.Text) -> Preprocessor
substituteprefix pat with p n = do
  with' <- with p
  pure . modifyNode n $ \node -> case withProxyNode (Proxy @Primitive) node of
    Just (NodeString _ SingleQuote) -> node
    Just (NodeString t q) -> Node $ NodeString t' q
      where
        t' = case T.stripPrefix pat t of
          Nothing -> t
          Just x  -> with' <> x
    _ -> node


replaceN :: Int -> T.Text -> T.Text -> T.Text -> T.Text
replaceN 0 _ _ t = t
replaceN x pat with input = bool (error "negative number of replaces in replaceN") (left <> right) (x>0)
  where
    (left', right') = T.breakOn pat input
    (left, right) = second (replaceRight . T.stripPrefix pat) (left', right')
    replaceRight (Just y) = with <> replaceN (x-1) pat with y
    replaceRight Nothing = right'


envVarPreprocessor :: Preprocessor
envVarPreprocessor _p node = case withProxyNode (Proxy @Primitive) node of
  Just (NodeString _ SingleQuote) -> pure node
  Just (NodeString s q) -> case runParser (many substringParser) s of
      Just (leftover, xs) -> Node . (`NodeString` q) . (<>leftover) . T.concat <$> sequence xs
      Nothing -> pure . Node $ NodeString s q
  _ -> pure node
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
