module Data.SmartCard.Ki where

import Data.Word

data Ki = Ki [Word8]
        deriving (Show)