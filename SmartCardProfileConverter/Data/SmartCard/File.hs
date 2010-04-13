module Data.SmartCard.File where

import Data.SmartCard.File.Types

data File = MasterFile FileID
          | DedicatedFile  FileID FileID
          | ElementaryFile FileID FileID Bool Bool EFAccessCondition EFAccessCondition EFAccessCondition EFAccessCondition EFAccessCondition EFContent
          deriving (Show)
-- MasterFile
--   FileID

-- DedicatedFile
--   FileID
--   ParentID

-- ElementaryFile
--   FileID
--   ParentID
--   Invalidated
--   AccessibleWhenInvalidated
--   readAccessCondition
--   updateAccessCondition
--   increaseAccessCondition
--   invalidateAccessCondition
--   rehabilitateAccessCondition
