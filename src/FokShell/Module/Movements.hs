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

data Movements = Movements
  {
    jumpMod :: Maybe KeyModifiers
  }
instance Def Movements where
  def = Movements
    { jumpMod = Just control
    }
instance Module' Movements ShellProcess where
  initHook' tc p = pure (tc,p)
  resetHook' tc p = pure (tc,p)
  exitHook' tc p = pure (tc,p)
  preHook' tc p (modf,Arrow d) = (\x -> bool (pure (True,(tc,p))) x $ d `elem` [DLeft, DRight]) $ 
    bool 
    (pure (True,(tc,p)))
    (moveCursor' p.shellConfig d (abs $ n d)
        $> (True, (tc,p {shellConfig =
          p.shellConfig {cursorLoc = p.shellConfig.cursorLoc + n d}})))
    (Just modf == tc.jumpMod)
    where
      cursor = T.length p.shellConfig.input - p.shellConfig.cursorLoc
      (left, right) = T.splitAt cursor p.shellConfig.input
      n DLeft = case T.takeWhileEnd isSpace left of
        "" ->  case reverse $ T.words left of
          (x:_) -> T.length x
          _ -> 0
        x -> T.length x
      n DRight = case T.takeWhile isSpace right of
        "" -> case T.words right of
          (x:_) -> -T.length x
          _ -> 0
        x -> -T.length x
      n _ = undefined
  preHook' tc p _ = pure (True, (tc,p))
  postHook' tc p _ = pure (True,(tc,p))
