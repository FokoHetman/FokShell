{-# LANGUAGE OverloadedStrings, LambdaCase, AllowAmbiguousTypes, ScopedTypeVariables, GADTs, ExplicitNamespaces #-}
module FokShell.Module.Parser where
import Language.Parser
import Lib.Primitive

import Control.Applicative ((<|>), liftA)
import Data.Text qualified as T

import FokShell.Module
--import FokShell.Module.Preprocessor
--import FokShell.Module.Preprocessor.StringPreprocessors
import System.Directory (getHomeDirectory)
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import GHC.InfoProv (whereFrom)
import FokShell.Types
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Environment.Blank (getEnvironment, getEnv)
import Data.Maybe (fromMaybe)
import FokShell.Types
import System.IO (stdin, stdout, stderr, openFile, IOMode (WriteMode, AppendMode))
import Data.Bool (bool)
import GHC.IO.Exception qualified as E
import qualified Control.Exception as E
import Control.Monad.Error.Class (MonadError(throwError))
import Data.Data (eqT, type (:~:) (Refl), typeRep, Proxy (Proxy))
import FokShell.Utils (getMaybe)
import FokShell.Module.Builtins (Builtins (Builtins))
import Control.Concurrent.STM (readTVarIO)
import qualified Data.Map as M
import FokShell.Builtin.Raw (rawProcess)
{-
-- parser definition {{{
newtype Parser a = Parser {runParser :: T.Text -> Maybe (T.Text, a)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input',x) <- p input
      Just (input',f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a)<- p2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  Parser pa >>= f = Parser $ \input -> do
    (rest, a) <- pa input
    runParser (f a) rest
-- }}}

-- helpers {{{
ws :: Parser T.Text
ws = spanP isSpace

wsForce = do
  s <- spanP isSpace
  if T.null s then empty else pure s

spanP, spanPForce :: (Char -> Bool) -> Parser T.Text
spanP f = Parser $ \input ->
  let (token, rest) = T.span f input
  in Just (rest, token)

spanPForce f = do
  res <- spanP f
  bool (pure res) empty (T.null res)

item :: Parser Char
item = Parser $ \t -> swap <$> T.uncons t

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  if p c then pure c else empty

charP :: Char -> Parser Char
charP c = satisfy (==c)
-- }}}
-}

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

type Parser = Parsec Void T.Text

data Node a = Node Span a deriving (Eq,Ord,Show)
instance Resolvable a => Resolvable (Node a) where
  resolve (Node _ a) = resolve a

instance Pipelineable (Node PipelineBuilder) where
  buildPipeline (Node _ t) = buildPipeline t
instance CommandNode a => CommandNode (Node a) where
  getCommand (Node (Span a b) t) i = bool
    Nothing
    (getCommand t i)
    (i>=a && i<=b)
  getPath (Node (Span a b) t) i = bool
    Nothing
    (getPath t i)
    (i>=a && i<=b)


pSpan :: Parser a -> Parser (Node a)
pSpan p = do
  start <- getOffset
  capture <- p
  end <- getOffset
  pure $ Node (Span start end) capture

class Parseable a where
  parseNode :: Parser a

class Pipelineable a where
  buildPipeline  :: a -> ShellAction SomePipeline
class CommandNode a where
  getCommand:: a -> Int -> Maybe ProcessCall
  getPath   :: a -> Int -> Maybe Path

data PipelineBuilder where PipelineBuilder :: (Show a, Pipelineable a, CommandNode a) => a -> PipelineBuilder

instance Pipelineable PipelineBuilder where
  buildPipeline (PipelineBuilder a) = buildPipeline a
instance CommandNode PipelineBuilder where
  getCommand (PipelineBuilder a) = getCommand a
  getPath (PipelineBuilder a) = getPath a
instance Show PipelineBuilder where
  show (PipelineBuilder a) = show a

data Sequence = Sequence (E.ExitCode -> ShellAction Bool) (Node PipelineBuilder) (Node PipelineBuilder)
instance Pipelineable Sequence where
  buildPipeline (Sequence condition (Node (Span sl _) left) (Node (Span _ sr) right)) = do
    SomePipeline (l :: Pipeline i o) <- buildPipeline left
    SomePipeline (r :: Pipeline i2 o2) <- buildPipeline right
    case eqT @i @i2 of
      Just Refl -> case eqT @o @o2 of
        Just Refl -> pure . SomePipeline $ Followed l r condition
        _ -> throwError $ PipelineTypeError (Span sl sr) $ "Incompatible output types: `" <> (T.show $ typeRep (Proxy @o)) <> "` != `" <> (T.show $ typeRep (Proxy @o2)) <> "`"
      _ -> throwError $ PipelineTypeError (Span sl sr) $ "Incompatible input types: `" <> (T.show $ typeRep (Proxy @i)) <> "` != `" <> (T.show $ typeRep (Proxy @i2)) <> "`"
instance CommandNode Sequence where
  getCommand (Sequence _ n1 n2) i = getCommand n1 i <|> getCommand n2 i
  getPath (Sequence _ n1 n2) i = getPath n1 i <|> getPath n2 i

instance Show Sequence where
  show (Sequence _ t1 t2) = "Sequence (" <> show t1 <> "|" <> show t2 <> ")"

pSequence :: (E.ExitCode -> ShellAction Bool) -> Parser a -> Parser PipelineBuilder -> Parser Sequence
pSequence condition joint lower = Sequence condition <$> lexeme (pSpan lower) <*> (lexeme joint *> lexeme (pSpan ((PipelineBuilder <$> try (pSequence condition joint lower)) <|> lower)))

pAndSequence, pOrSequence, pSentenceSequence :: Parser PipelineBuilder -> Parser Sequence
pAndSequence   = pSequence (pure . (==E.ExitSuccess)) (string "&&")
pOrSequence    = pSequence (pure . (==E.ExitSuccess)) (string "||")
pSentenceSequence = pSequence (pure . (==E.ExitSuccess)) (satisfy (`elem` (";\n" :: String)))
{-
data Pipe = Pipe (DataPath -> Pipeline -> Pipeline) IOMode (Node PipelineBuilder) (Node Path)
instance Show Pipe where
  show (Pipe _ m n p) = "Pipe (" <> show m <> "," <> show n <> "," <> show p <> ")"
instance Pipelineable Pipe where
  buildPipeline (Pipe f mode (Node _ t) (Node _ path)) = do
    t' <- buildPipeline t
    rpath <- resolve path
    h <- liftIO (E.try (openFile (T.unpack rpath) mode)) >>= \case
      Left (e :: IOError) -> throwError $ ErrorIO e
      Right h -> pure h
    pure $ f (FileDescriptor h) t'
instance CommandNode Pipe where
  getCommand (Pipe _ _ t _) = getCommand t
  getPath (Pipe _ _ t p) i = getPath t i <|> getPath p i

pPipe :: (DataPath -> Pipeline -> Pipeline) -> IOMode -> Parser a -> Parser PipelineBuilder -> Parser Path -> Parser Pipe
pPipe f mode joint lower path = Pipe f mode <$> lexeme (pSpan lower) <*> (lexeme joint *> lexeme (pSpan path))

-}

data ProcessPipeline = ProcessPipeline (Node PipelineBuilder) (Node PipelineBuilder) deriving Show

instance Pipelineable ProcessPipeline where
  buildPipeline (ProcessPipeline (Node (Span l _) left) (Node (Span _ r) right)) = do
    SomePipeline (left' :: Pipeline i m1) <- buildPipeline left
    SomePipeline (right':: Pipeline m2 o) <- buildPipeline right
    case eqT @m1 @m2 of
      Just Refl -> pure $ SomePipeline $ left' >-> right'
      _ -> throwError $ PipelineTypeError (Span l r) "mismatch in process pipeline"

instance CommandNode ProcessPipeline where
  getCommand (ProcessPipeline (Node (Span l1 r1) (PipelineBuilder l)) (Node (Span l2 r2) (PipelineBuilder r))) i = bool
    (bool
      Nothing
      (getCommand r i)
      (i>=l2 && i<=r2)
    )
    (getCommand l i)
    (i>=l1 && i<=r1)
  getPath (ProcessPipeline (Node (Span l1 r1) (PipelineBuilder l)) (Node (Span l2 r2) (PipelineBuilder r))) i = bool
    (bool
      Nothing
      (getPath r i)
      (i>=l2 && i<=r2)
    )
    (getPath l i)
    (i>=l1 && i<=r1)


pPipeline :: Parser PipelineBuilder -> Parser ProcessPipeline
pPipeline lower = ProcessPipeline <$> lexeme (pSpan lower) <*> (lexeme (char '|') *> lexeme (pSpan $ (try (PipelineBuilder <$> pPipeline lower)) <|> lower))

data ProcessCall = ProcessCall (Node Path) [Node Path] deriving Show
pPCall :: Parser Path -> Parser ProcessCall
pPCall path = ProcessCall <$> lexeme (pSpan path) <*> many (lexeme $ pSpan path)


instance Pipelineable ProcessCall where
  buildPipeline (ProcessCall name args) = do
    processName <- resolve name
    processArgs <- mapM resolve args
    SomeProcess p <- getMaybe @Builtins >>= \case
      (Just (Builtins m)) -> do
        m' <- liftIO $ readTVarIO m
        pure $ case M.lookup processName m' of
          Just builtin -> builtin
          Nothing -> SomeProcess $ rawProcess processName
      Nothing -> pure $ SomeProcess $ rawProcess processName
    pure . SomePipeline $ Stage p processArgs
instance CommandNode ProcessCall where
  getCommand x _ = Just x
  getPath (ProcessCall name args) i = getPath name i <|> foldl (<|>) empty (fmap (`getPath` i) args)



data Path = Path (Maybe (Node Root)) [Node PathComponent] deriving (Show)
instance CommandNode Path where
  getCommand _ _ = Nothing
  getPath x _ = Just x

pPath :: Parser Root -> Parser PathComponent -> Parser Path
pPath root comp = do
  r <- optional $ pSpan root
  comps <- (pSpan comp) `sepBy` char '/'
  _ <- optional $ char '/'
  case (r, comps) of
    (Nothing, []) -> empty
    _ -> pure $ Path r comps

instance Resolvable Path where
  resolve (Path root comps) = do
    comps' <- mapM resolve comps
    case root of
      Just x -> do
        x' <- resolve x
        pure $ T.intercalate "/" $ x':comps'
      Nothing-> pure $ T.intercalate "/" comps'


data Root where Root :: (Show a, Resolvable a) => a -> Root
instance Show Root where
  show (Root a) = show a
instance Resolvable Root where
  resolve (Root a) = resolve a

data RootRoot = RootRoot deriving Show
pRoot :: Parser Root
pRoot = char '/' *> pure (Root RootRoot)
instance Resolvable RootRoot where resolve _ = pure "/"
data HomeRoot = HomeRoot deriving Show
pHome :: Parser Root
pHome = char '~' *> optional (char '/') *> pure (Root HomeRoot)
instance Resolvable HomeRoot where resolve _ = T.pack <$> liftIO getHomeDirectory

class Resolvable a where
  resolve :: a -> ShellAction T.Text

data PathComponent where PathComponent :: (Resolvable a, Show a) => a -> PathComponent
instance Show PathComponent where
  show (PathComponent a) = show a

instance Resolvable PathComponent where
  resolve (PathComponent a) = resolve a

defaultBlockers :: String
defaultBlockers = " \t\n|&;()<>\"'`$\\"

data Literal = Literal T.Text deriving Show
pLiteral :: String -> Parser Literal
pLiteral blockers = Literal <$> (T.pack <$> some (satisfy (`notElem` blockers)) <* notFollowedBy (char '>'))

instance Resolvable Literal where
  resolve (Literal t) = pure t


{-

pPcall :: Parser ProcessCall
pPcall = undefined


pPath :: Grammar -> Parser Path
pPath g = Path <$> pComponent `sepBy` char '/' <* optional (char '/')
  where pComponent = pSpan (g.component g)

class Resolvable a where
  resolve :: a -> ShellAction T.Text

data PathComponent where PathComponent :: (Resolvable a, Show a) => a -> PathComponent
instance Show PathComponent where
  show (PathComponent a) = show a

instance Resolvable PahComponent where
  resolve (PathComponent a) = resolve a


data Literal = Literal T.Text
pLiteral :: Grammar -> Parser Literal
pLiteral _ = Literal <$> (T.pack <$> some alphaNumChar)

instance Resolvable Literal where
  resolve (Literal t) = pure t


data InlineProcessCall = InlineProcessCall ProcessCall
pInlinePcall :: Grammar -> Parser InlineProcessCall
pInlinePcall _ = InlineProcessCall <$> ((char '$') *> parens pPcall)

instance Resolvable InlineProcessCall where
  resolve (InlineProcessCall pcall) = do
    undefined --executeCommand pcall


data EnvVar = EnvVar T.Text
pEnvVar :: Grammar -> Parser EnvVar
pEnvVar _ = EnvVar . T.pack <$> (char '$' *> some alphaNumChar)

instance Resolvable EnvVar where
  resolve (EnvVar e) = liftIO $ T.pack . fromMaybe "" <$> getEnv (T.unpack e)
-}

{-
data PathComponent =
    PathLiteral Literal
  | Quoted  T.Text
  | DQuoted [Node Literal]
  | Home
  | Root
  deriving (Eq,Ord,Show)

pPathComponent :: Parser PathComponent
pPathComponent = (PathLiteral <$> pLiteral) <|> try quoted <|> try dquoted <|> try home
  where
    quoted :: Parser PathComponent
    quoted = Quoted <$> (between (symbol "'") (symbol "'") (T.pack <$> (many $ satisfy (/='\''))))
    dquoted = DQuoted <$> (between (symbol "\"") (symbol "\"") (many $ pSpan pLiteral))
    home = char '~' *> pure Home
-}
{-data Literal =
    StringLiteral T.Text
  | InlineProcessCall ProcessCall
  | EnvVar T.Text
  deriving (Eq,Ord,Show)

pLiteral :: Parser Literal
pLiteral = pStringLiteral <|> try inlineCall <|> try envVar
  where
  pStringLiteral = StringLiteral <$> (T.pack <$> some alphaNumChar)
  inlineCall = InlineProcessCall <$> ((char '$') *> parens pPcall)
  envVar = EnvVar . T.pack <$> (char '$' *> some alphaNumChar)-}

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

data ParserConfig = ParserConfig
  { parser :: Parser PipelineBuilder
  }

instance Def ParserConfig where
  def = ParserConfig {parser = statementSequence <* eof}
    where
    statementSequence, andSequence, orSequence, pcall :: Parser PipelineBuilder
    statementSequence = try (PipelineBuilder <$> pSentenceSequence orSequence) <|> orSequence
    orSequence = try (PipelineBuilder <$> pOrSequence andSequence) <|> andSequence
    andSequence = try (PipelineBuilder <$> pAndSequence pipeSequence) <|> pipeSequence
    pipeSequence = try (PipelineBuilder <$> pPipeline pcall) <|> pcall
    {-writePipe-}
    --writePipe = try (PipelineBuilder <$> pPipe (\x t -> t{dataOut = x}) WriteMode (char '>') errPipe path) <|> errPipe
    --errPipe = try (PipelineBuilder <$> pPipe (\x t -> t{dataErr = x}) WriteMode (string "2>") bothPipe path) <|> bothPipe
    --bothPipe = try (PipelineBuilder <$> pPipe (\x t -> t{dataErr = x}) WriteMode (string "&>") appendPipe path) <|> appendPipe
    --appendPipe = try (PipelineBuilder <$> pPipe (\x t -> t{dataOut = x}) AppendMode (string ">>") appendErrPipe path) <|> appendErrPipe
    --appendErrPipe = try (PipelineBuilder <$> pPipe (\x t -> t{dataErr = x}) AppendMode (string "2>>") appendBothPipe path) <|> appendBothPipe
    --appendBothPipe = try (PipelineBuilder <$> pPipe (\x t -> t{dataOut = x, dataErr = x}) AppendMode (string "&>>") pcall path) <|> pcall
    pcall = PipelineBuilder <$> pPCall path
    path = pPath (pHome <|> pRoot) (PathComponent <$> pLiteral defaultBlockers)

data ParserModule = ParserModule
  { parser :: Parser PipelineBuilder
  }

instance Configurable ParserModule ParserConfig where
  configure conf = pure ParserModule {parser = conf.parser}

instance Module' ParserModule ShellAction where
  initHook _ = pure ()
  exitHook _ = pure ()
  resetHook _ = pure True
  preHook _ _ = pure True
  postHook _ _ = pure True

{-
data ParserModule = ParserModule
  { parser :: Parser Node
  --, preprocessors :: [Preprocessor]
  }

instance Def ParserModule where
  def = ParserModule
    { parser = r4
    --, preprocessors = [connectPreprocessors [substituteprefix "~" (const $ T.pack <$> getHomeDirectory), envVarPreprocessor]]
    }
r0,r1,r2,r3,r4 :: Parser Node
r0 = primitives empty
r1 = pcall r0 <|> r0
r2 = pipes r1 <|> r1
r3 = chains r2 <|> r2
r4 = detach r3 <|> r3

instance Module' ParserModule ShellAction where
  initHook' _ = pure ()
  exitHook' _ = pure ()
  resetHook' _ = pure True
  preHook' _ _ = pure True
  postHook' _ _ = pure True

primitives, pcall, pipes, chains, detach :: Parser Node -> Parser Node
primitives lower = Node <$> (parse lower :: Parser Primitive)
pcall lower = Node <$> (parse lower :: Parser ProcessCall)
pipes lower = Node <$> (parse lower :: Parser PipelineExp)
chains lower = Node <$> (parse lower :: Parser ChainExp)
detach lower = Node <$> (parse lower :: Parser Detach)-}
-- vim: foldmethod=marker
