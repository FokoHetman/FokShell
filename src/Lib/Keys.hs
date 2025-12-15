module Lib.Keys where

import qualified Data.Text as T
import qualified Data.Bits as B

import Lib.Format

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

