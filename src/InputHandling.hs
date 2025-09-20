{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module InputHandling where

import qualified Data.Text as T
import qualified Data.Bits as B
import System.IO (hWaitForInput, stdin)
import Data.Char (chr, ord)
import Data.List (singleton)

data Direction = Up | Down | DRight | DLeft
    deriving (Show,Eq)
data KeyCode = Fn | Escape | Arrow Direction | Enter | Tab | Backspace | Delete | Character T.Text
    deriving (Show,Eq)
newtype KeyModifiers = KeyModifiers Int
    deriving (Show,Eq)
control :: KeyModifiers
control = KeyModifiers 1  -- 2^0

shift :: KeyModifiers
shift = KeyModifiers 2    -- 2^1

alt :: KeyModifiers
alt = KeyModifiers 4      -- 2^2

(.|.) :: KeyModifiers -> KeyModifiers -> KeyModifiers
(KeyModifiers a) .|. (KeyModifiers b) = KeyModifiers (a B..|. b)

type KeyEvent = (KeyModifiers, KeyCode)


nextEvent :: IO KeyEvent
nextEvent = do
  --print input
  stringToKeyEvent <$> getInputString

  --pure (KeyModifiers 0, Escape)

getInputString :: IO String
getInputString = do
  getChar >>= \case 
    '\ESC' -> do
      rest <- getLoop
      pure $ '\ESC':rest
    x      -> pure [x]
  where
    getLoop = do
      more <- hWaitForInput stdin 20
      if more then do
        char <- getChar
        rest <- getLoop
        pure $ char:rest
      else
        pure ""

stringToKeyEvent :: String -> KeyEvent
stringToKeyEvent x
          | head x == '\ESC'  = case x!!1 of
            '[' -> case x!!2 of
              '1' -> if x!!3 == ';' then (modifierMatch $ x!!4, arrowMatch $ x!!5)  else unknown
              a   -> (KeyModifiers 0, arrowMatch a)
            _  -> (KeyModifiers 0, Escape)
          | length x == 1 = charMatch $ head x
          | otherwise         = (KeyModifiers 0, Character $ T.pack x)
  where
    arrowMatch 'A' = Arrow Up
    arrowMatch 'B' = Arrow Down
    arrowMatch 'C' = Arrow DRight
    arrowMatch 'D' = Arrow DLeft
    arrowMatch '2' = Fn
    arrowMatch '3' = Delete
    arrowMatch x = error [x]

    modifierMatch '2' = shift
    modifierMatch '3' = alt
    modifierMatch '4' = alt .|. shift
    modifierMatch '5' = control
    modifierMatch '6' = control .|. shift
    modifierMatch '7' = control .|. alt
    modifierMatch '8' = alt .|. control .|. shift
    modifierMatch _ = undefined

    charMatch :: Char -> KeyEvent
    charMatch ch
              | '\DEL' == ch          = (KeyModifiers 0, Backspace)
              | '\ESC' == ch        = (KeyModifiers 0, Escape)
              | '\t' == ch          = (KeyModifiers 0, Tab)
              | '\n' == ch          = (KeyModifiers 0, Enter)
              | 20 >= ord ch && ord ch > 0 = (control, Character $ T.pack $ singleton $ chr $ ord ch + 96)
              | 254 >= ord ch && ord ch > 224 = (alt, Character $ T.pack $ singleton $ chr $ ord ch - 128)
              | otherwise = (KeyModifiers 0, Character $ T.pack $ singleton ch)
    unknown = (KeyModifiers 0, Escape)
