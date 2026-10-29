{-# LANGUAGE OverloadedStrings, LambdaCase, ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
module FokShell.ShellNode.Table where
import Data.List (transpose, intercalate)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Type.Reflection
import Data.Kind
import Prettyprinter
import Prettyprinter.Render.Terminal
import FokShell.ShellNode
import Data.Text qualified as T
import GHC.Base (Symbol)
import Data.Data (Proxy (Proxy))
import Data.Bool (bool)

data Cols (cols :: [Type]) where
  CEmpty :: Cols '[]
  CCons  :: PrettyAnn ShellAnn col => col -> Cols rest -> Cols (col ': rest)

infixr 1 >::
(>::) :: PrettyAnn ShellAnn col => col -> Cols rest -> Cols (col ': rest)
(>::) = CCons

type family Schema (schema :: [(Symbol, Type)]) :: [Type] where
  Schema '[] = '[]
  Schema ('(name, ty) ': xs) = ty ': Schema xs

newtype Row schema = Row (Cols (Schema schema))
newtype Table schema = Table [Row schema]

data SomeTable where SomeTable :: KnownSchema schema => Table schema -> SomeTable

columnLengths :: Cols cols -> [Int]
columnLengths CEmpty = []
columnLengths (CCons x rest) = T.length (renderStrict . layoutPretty defaultLayoutOptions . unAnnotate $ prettyAnn @ShellAnn x) : columnLengths rest

prettyRow :: [Int] -> Cols cols -> Doc ShellAnn
prettyRow (x:xs) (CCons y ys) = hcat [prettyAnn ("│ " :: T.Text), fill x (prettyAnn y), prettyAnn (" " :: T.Text), prettyRow xs ys]
prettyRow _ _ = prettyAnn ("│" :: T.Text)


class KnownSchema (schema :: [(Symbol, Type)]) where
  schemaStrings :: Proxy schema -> [String]

instance KnownSchema '[] where
  schemaStrings _ = []

instance (Typeable t, KnownSymbol name, KnownSchema rest) => KnownSchema ('(name, t) ': rest) where
  schemaStrings _ = symbolVal (Proxy @name) : schemaStrings (Proxy @rest)

instance PrettyAnn ShellAnn SomeTable where
  prettyAnn (SomeTable (Table @schema x)) = pageWidth $ \case
    AvailablePerLine l _ -> build (f l colLengths)
    _ -> build colLengths
    where
    lineSep ls = (prettyAnn $ "├" <> (T.pack . intercalate "┼" $ fmap (\n -> take (n+2) (repeat '─')) ls) <> "┤")
    build colLengths' = (<> "\n")
      $ vsep
      $ (prettyAnn @ShellAnn ("┌" <> (T.pack . intercalate "┬" $ fmap (\n -> take (n+2) (repeat '─')) colLengths') <> "┐"):)
      $ (<> [prettyAnn @ShellAnn ("└" <> (T.pack . intercalate "┴" $ fmap (\n -> take (n+2) (repeat '─')) colLengths') <> "┘")])
      $ (\case
        (l:ls) -> l:lineSep colLengths':ls
        a -> a
        )
      -- $ punctuate (prettyAnn $ "\n├" <> (T.pack . intercalate "┼" $ fmap (\n -> take (n+2) (repeat '─')) colLengths') <> "┤")
      $ (hcat (prettyAnn ("│" :: T.Text):fmap (\(i,v) -> hcat [prettyAnn (" " :: T.Text), fill i $ annotate AnnColumn $ prettyAnn v, prettyAnn (" │" :: T.Text)]) (zip colLengths' header)):)
      $ fmap (\(Row c) -> prettyRow colLengths' c) x
    f :: Int -> [Int] -> [Int]
    f _ [] = []
    f i (y:ys) = bool
      ([])
      (y : f (i-y-3) ys)
      (i > y+3)
    header = schemaStrings (Proxy @schema)
    colLengths = fmap maximum . transpose $ fmap length header : fmap (columnLengths . (\(Row r) -> r)) x
