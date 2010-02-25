module Data.SmartCard.DedicatedFile where

import Data.SmartCard.File

class (File a) => DedicatedFile a where
  getContents :: (File b) => a -> b
-- add security content