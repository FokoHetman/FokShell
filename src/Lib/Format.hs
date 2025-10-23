module Lib.Format where

import qualified Data.Text as T
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

moveCursor :: Direction -> Int -> IO ()
moveCursor _ 0 = pure ()
moveCursor DLeft i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "D"
moveCursor DRight i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "C"
moveCursor Up i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "A"
moveCursor Down i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "B"

data Direction = Up | Down | DRight | DLeft
    deriving (Show,Eq)

-- DISPLAY FUNCTIONS
eputStrf :: IO T.Text -> IO ()
eputStrf t = t >>= \x -> putStr (T.unpack x) <> hFlush stdout

putStrf :: T.Text -> IO ()
putStrf t = putStr (T.unpack t) <> hFlush stdout


