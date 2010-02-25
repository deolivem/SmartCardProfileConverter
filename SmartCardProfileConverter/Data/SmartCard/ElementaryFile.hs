module Data.SmartCard.ElementaryFile where

import Data.SmartCard.File

-- the structure of Elementary File
data FileStructure = Transparent
                   | LinearFixed
                   | Cyclic
                   deriving (Show, Eq)

                   
class File a => ElementaryFile a where
  getStructure :: a -> FileStructure
-- add security content