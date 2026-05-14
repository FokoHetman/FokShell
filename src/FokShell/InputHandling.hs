{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module FokShell.InputHandling where

import qualified Data.Text as T
import System.IO (hWaitForInput, stdin)
import Data.Char (chr, ord, digitToInt)
import Data.List (singleton, isPrefixOf)

import Lib.Keys
import Lib.Format
import Debug.Trace (traceShow)

nextEvent :: IO KeyEvent
nextEvent = stringToKeyEvent <$> getInputString

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
stringToKeyEvent ('\ESC':'[':'1':';':modifier:key) = (getMod modifier, getKey key)
stringToKeyEvent ('\ESC':'[':xs)
    | xs == "Z" = (shift, Tab)
    | otherwise = (KeyModifiers 0, getKey xs)
stringToKeyEvent ('\ESC':'O':xs) = stringToKeyEvent ('\ESC':'[':xs)
stringToKeyEvent "\DEL" = (KeyModifiers 0, Backspace)
stringToKeyEvent "\ESC" = (KeyModifiers 0, Escape)
stringToKeyEvent "\t" = (KeyModifiers 0, Tab)
stringToKeyEvent "\n" = (KeyModifiers 0, Enter)
stringToKeyEvent (ch:_)
              | 20 >= ord ch && ord ch > 0 = (control, Character $ T.pack $ singleton $ chr $ ord ch + 96)
              | 254 >= ord ch && ord ch > 224 = (alt, Character $ T.pack $ singleton $ chr $ ord ch - 128)
              | otherwise = (KeyModifiers 0, Character $ T.pack $ singleton ch)
stringToKeyEvent "" = (KeyModifiers 0, Character "")
getMod c = KeyModifiers $ digitToInt c - 1

getKey "A" = Arrow Up
getKey "B" = Arrow Down
getKey "C" = Arrow DRight
getKey "D" = Arrow DLeft
getKey "2~" = Fn
getKey "3~" = Delete
getKey "1~"  = Home
getKey "4~"  = End
getKey "5~"  = Page Up
getKey "6~"  = Page Down
getKey "15~" = F 5
getKey "17~" = F 6
getKey "18~" = F 7
getKey "19~" = F 8
getKey "20~" = F 9
getKey "21~" = F 10
getKey "23~" = F 11
getKey "24~" = F 12
getKey "H" = Home
getKey "F" = End
getKey "P" = F 1
getKey "Q" = F 2
getKey "R" = F 3
getKey "S" = F 4
getKey "Z" = Tab
getKey _x = traceShow ("unknown character", _x) $ Character ""
{-stringToKeyEvent x = (KeyModifiers 0, Character $ T.pack x)
          | head x == '\ESC' && length x > 1= case x!!1 of
            '[' -> case x!!2 of
              '1' -> if x!!3 == ';' then (modifierMatch $ x!!4, getKey $ x!!5)  else unknown
              a   -> (KeyModifiers 0, getKey a)
            _  -> (KeyModifiers 0, Escape)
          | head x == '\ESC' = (KeyModifiers 0, Escape)
          | length x == 1 = charMatch $ head x
          | otherwise         = (KeyModifiers 0, Character $ T.pack x)
  where
    modifierMatch '2' = shift
    modifierMatch '3' = alt
    modifierMatch '4' = alt .|. shift
    modifierMatch '5' = control
    modifierMatch '6' = control .|. shift
    modifierMatch '7' = control .|. alt
    modifierMatch '8' = alt .|. control .|. shift
    modifierMatch _ = KeyModifiers 0

    charMatch :: Char -> KeyEvent
    charMatch ch
              | '\DEL' == ch          = (KeyModifiers 0, Backspace)
              | '\ESC' == ch        = (KeyModifiers 0, Escape)
              | '\t' == ch          = (KeyModifiers 0, Tab)
              | '\n' == ch          = (KeyModifiers 0, Enter)
              | 20 >= ord ch && ord ch > 0 = (control, Character $ T.pack $ singleton $ chr $ ord ch + 96)
              | 254 >= ord ch && ord ch > 224 = (alt, Character $ T.pack $ singleton $ chr $ ord ch - 128)
              | otherwise = (KeyModifiers 0, Character $ T.pack $ singleton ch)
    unknown = (KeyModifiers 0, Escape)-}
