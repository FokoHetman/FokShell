{-# LANGUAGE OverloadedStrings #-}
module FokShell.Module.Movements where
import Lib.Keys
import Lib.Primitive
import FokShell.Module
import FokShell.Types
import Lib.Format (Direction(DLeft, DRight))
import Data.Bool (bool)
import FokShell.Utils (moveCursor')
import Data.Functor
import Data.Text qualified as T
import Data.Char (isSpace)
import Control.Concurrent.STM

data Movements = Movements
  {
    jumpMod :: Maybe KeyModifiers
  }
instance Def Movements where
  def = Movements
    { jumpMod = Just control
    }
instance Module' Movements ShellConfig where
  initHook' _tc _p = pure ()
  resetHook' _tc _p = pure True
  exitHook' _tc _p = pure ()
  preHook' tc conf (modf,Arrow d) = do
    config <- readTVarIO conf
    tc' <- readTVarIO tc
    let cursor = T.length config.input - config.cursorLoc
        (left,right) = T.splitAt cursor config.input
    let ret = bool (pure True) (moveCursor' conf d (abs $ n left right d) $> True) (Just modf == tc'.jumpMod)
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
  preHook' _tc _p _ = pure True
  postHook' _tc _p _ = pure True
