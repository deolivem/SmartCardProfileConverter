module Data.SmartCard.Ki where

import Data.SmartCard.Class.Default
import Data.Word

data Ki = Ki [Word8]
        deriving (Show)

instance Default Ki where
  defaultValue = Ki []