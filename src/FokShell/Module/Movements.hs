{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Movements where
import Lib.Keys
import Lib.Primitive
import FokShell.Module
import FokShell.Types
import Lib.Format (Direction(DLeft, DRight))
import Data.Bool (bool)
import FokShell.Utils
import Data.Functor
import Data.Text qualified as T
import Data.Char (isSpace)
import Control.Concurrent.STM
import FokShell.Module.Input
import Control.Monad.IO.Class (MonadIO(liftIO))

data Movements = Movements
  {
    jumpMod :: Maybe KeyModifiers
  }
instance Def Movements where
  def = Movements
    { jumpMod = Just control
    }
instance Module' Movements ShellAction where
  initHook' _tc = pure ()
  resetHook' _tc = pure True
  exitHook' _tc = pure ()
  preHook' m (modf,Arrow d) = do
    inputM <- get @Input
    input <- liftIO $ readTVarIO inputM.input
    cursorLocation <- liftIO $ readTVarIO inputM.cursorLocation
    let cursor = T.length input - cursorLocation
        (left,right) = T.splitAt cursor input
    let ret = bool (pure True) (moveInputCursor inputM d (abs $ n left right d) $> True) (Just modf == m.jumpMod)
    bool (pure True) ret $ d `elem` [DLeft, DRight]
    where
      n left _ DLeft = case T.takeWhileEnd isSpace left of
        "" ->  case reverse $ T.words left of
          (x:_) -> T.length x
          _ -> 0
        x -> T.length x
      n _ right DRight = case T.takeWhile isSpace right of
        "" -> case T.words right of
          (x:_) -> -T.length x
          _ -> 0
        x -> -T.length x
      n _ _ _ = undefined
  preHook' _tc _ = pure True
  postHook' _tc _ = pure True
