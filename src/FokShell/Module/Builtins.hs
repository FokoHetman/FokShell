{-# LANGUAGE  OverloadedStrings #-}
module FokShell.Module.Builtins where
import Control.Concurrent.STM (TVar, newTVarIO)
import FokShell.Types
import FokShell.Module
import qualified Data.Map as M
import qualified Data.Text as T
import Lib.Primitive
import FokShell.Builtin.Core (ls_, cd_)

data Builtins = Builtins
  { builtins :: TVar (M.Map T.Text SomeProcess)
  }

data BuiltinsConfig = BuiltinsConfig (M.Map T.Text SomeProcess)

instance Def BuiltinsConfig where
  def = BuiltinsConfig $ M.fromList
    [ ("ls", SomeProcess ls_)
    , ("cd", SomeProcess cd_)
    ]

instance Configurable Builtins BuiltinsConfig where
  configure (BuiltinsConfig m) = do
    m' <- newTVarIO m
    pure $ Builtins m'

instance Module' Builtins ShellAction where
  initHook _ = pure ()
  exitHook _ = pure ()
  resetHook _ = pure True
  preHook _ _ = pure True
  postHook _ _ = pure True
