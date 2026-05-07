module FokShell.Module.Cursor where
import Lib.Primitive
import FokShell.Module.Colorscheme
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import FokShell.Types (ShellProcess)
import FokShell.Module

import Data.Functor

data CursorModule = CursorModule
  {
    shape :: CursorShape
  , color :: Color
  }
instance Def CursorModule where
  def = CursorModule { shape = BlinkingBar, color = RGB 255 255 255 }

instance Module' CursorModule ShellProcess where
  initHook' tc p = mkCursor tc $> (tc, p)
  preHook' tc p _ = pure (True,(tc,p))
  postHook' tc p _ = pure (True,(tc,p))
  exitHook' tc p = pure (tc,p)

mkCursor :: CursorModule -> IO ()
mkCursor CursorModule{shape,color} = putStr (show shape <> cursorColor color) >> hFlush  stdout

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
