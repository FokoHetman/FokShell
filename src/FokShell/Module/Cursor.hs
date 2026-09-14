{-# LANGUAGE RecordWildCards #-}
module FokShell.Module.Cursor where
import Lib.Primitive
import FokShell.Module.Colorscheme
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import FokShell.Types (ShellConfig)
import FokShell.Module

import Data.Functor
import Control.Concurrent.STM (newTVar, TVar, atomically, readTVarIO)

data CursorConfig = CursorConfig
  {
    shape :: CursorShape
  , color :: Color
  }
instance Def CursorConfig where def = CursorConfig { shape = BlinkingBar, color = RGB 255 255 255 }

data Cursor = Cursor
  { shape :: TVar CursorShape
  , color :: TVar Color
  }

instance Configurable Cursor CursorConfig where
  configure conf = atomically $ do
    shape <- newTVar conf.shape
    color <- newTVar conf.color
    pure Cursor {..}

instance Module' Cursor ShellConfig where
  initHook' tc _ = mkCursor tc
  exitHook' _ _ = pure ()
  resetHook' tc _ = mkCursor tc $> True
  preHook' _ _ _ = pure True
  postHook' tc _ _ = mkCursor tc $> True

mkCursor :: Cursor -> IO ()
mkCursor Cursor{shape,color} = do
  color' <- readTVarIO color
  shape' <- readTVarIO shape
  putStr (show shape' <> cursorColor color') >> hFlush stdout

cursorColor :: Color -> String
cursorColor c = "\ESC]12;" <> show hex <> "\a"
  where 
    hex = toHex c

{-
ESC[0 q 	changes cursor shape to steady block
ESC[1 q 	changes cursor shape to steady block also
ESC[2 q 	changes cursor shape to blinking block
ESC[3 q 	changes cursor shape to steady underline
ESC[4 q 	changes cursor shape to blinking underline
ESC[5 q 	changes cursor shape to steady bar
ESC[6 q 	changes cursor shape to blinking bar
-}

data CursorShape = SteadyBlock | BlinkingBlock | SteadyUnderline | BlinkingUnderline | SteadyBar | BlinkingBar

instance Show CursorShape where
  show BlinkingBlock      = "\ESC[0 q"
  show SteadyBlock        = "\ESC[2 q"
  show BlinkingUnderline  = "\ESC[3 q"
  show SteadyUnderline    = "\ESC[4 q"
  show BlinkingBar        = "\ESC[5 q"
  show SteadyBar          = "\ESC[6 q"
