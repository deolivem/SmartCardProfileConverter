module Data.SmartCard.ATR where

import Data.Word
import Data.Hexadecimal

data ATR = ATR [Word8]

instance Show ATR where
  show (ATR xs) = "ATR " ++ _display xs
  
_display (x:[]) = toHex x
_display (x:xs) = toHex x ++ " " ++ _display xs