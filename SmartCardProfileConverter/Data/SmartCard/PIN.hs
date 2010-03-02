module Data.SmartCard.PIN where

import Data.Word

data PIN = 
  PIN { name :: String,
        initialized :: Bool,
        enable :: Bool,
        attemptsRemaining :: Integer,
        value :: [Word8]}
        deriving (Show)
