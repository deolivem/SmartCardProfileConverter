module Data.Hexadecimal where

import Data.Word

-- Class

class Hexadecimal a where
  toHex :: a -> String
  fromHex :: String -> a
  
showHex :: Hexadecimal a => a -> String
showHex = toHex

readHex :: Hexadecimal a => String -> a
readHex = fromHex

readHexaChar a = case a of
                   '0' -> 0
                   '1' -> 1
                   '2' -> 2
                   '3' -> 3
                   '4' -> 4
                   '5' -> 5
                   '6' -> 6
                   '7' -> 7
                   '8' -> 8
                   '9' -> 9
                   'a' -> 10
                   'A' -> 10
                   'b' -> 11
                   'B' -> 11
                   'c' -> 12
                   'C' -> 12
                   'd' -> 13
                   'D' -> 13
                   'e' -> 14
                   'E' -> 14
                   'f' -> 15
                   'F' -> 15
                   otherwise -> error "Not a hexadecimal string"

showHexaChar a = case a of
                    0  -> '0'
                    1  -> '1'
                    2  -> '2'
                    3  -> '3'
                    4  -> '4'
                    5  -> '5'
                    6  -> '6'
                    7  -> '7'
                    8  -> '8'
                    9  -> '9'
                    10 -> 'A'
                    11 -> 'B'
                    12 -> 'C'
                    13 -> 'D'
                    14 -> 'E'
                    15 -> 'F'
                    otherwise -> error "Not a hexadecimal value"

-- Instance

instance Hexadecimal Word8 where
  toHex v = let digit1 = showHexaChar $ v `div` 16
                digit2 = showHexaChar $ v `mod` 16
                in digit1:digit2:[]
                
  fromHex (x:y:_) = let digit1 = readHexaChar x
                        digit2 = readHexaChar y
                        in digit1*16+digit2
