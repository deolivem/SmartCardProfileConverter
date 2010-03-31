module Data.SmartCard.Class.Default where

-- class describing the fact than some type have default value
class Default a where
  defaultValue :: a

