module Data.SmartCard.ATR where

import Data.Word
import Data.Hexadecimal
import Data.SmartCard.Class.Default

data ATR = ATR [Word8]

instance Show ATR where
  show (ATR xs) = "ATR " ++ _display xs
  
_display [] = ""
_display (x:[]) = toHex x
_display (x:xs) = toHex x ++ " " ++ _display xs

instance Default ATR where
  defaultValue = ATR []