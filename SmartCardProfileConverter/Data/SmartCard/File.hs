module Data.SmartCard.File where

import Data.Word

data FileID = (Word8, Word8)

class File a where
  getFileID :: a -> FileID
  getParent :: a -> a
