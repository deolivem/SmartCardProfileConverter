module Data.SmartCard where

import Data.SmartCard.ATR
import Data.SmartCard.File
import Data.SmartCard.Ki
import Data.SmartCard.PIN

import Data.SmartCard.Class.Default


-- definition of the different parameters
data ClockStopMode = ClockStopNotAllowed
                   | ClockStopNotAllowedUnlessAtLowLevel
                   | ClockStopNotAllowedUnlessAtHighLevel
                   | ClockStopAllowedNoPreferredLevel
                   | ClockStopAllowedLowLevelPreferred
                   | ClockStopAllowedHighLevelPreferred
                   deriving (Show)

instance Default ClockStopMode where
  defaultValue = ClockStopNotAllowed
                   
data Voltage = FiveVolt
             | ThreeVolt
             | OneDotEightVolt
             deriving (Show)

instance Default Voltage where
  defaultValue = ThreeVolt
             
data Frequency = FourMHertz
               | EightMHertz
               deriving (Show)
             
instance Default Frequency where
  defaultValue = FourMHertz
             
data SmartCard =
  SmartCard { voltage :: Voltage,
              clockStopMode :: ClockStopMode,
              frequency :: Frequency,
              atr :: ATR,
              ki :: Ki,
              security :: [PIN],
              filesystem :: [File]}
              deriving (Show)

instance Default SmartCard where
  defaultValue = SmartCard defaultValue defaultValue defaultValue defaultValue defaultValue [] []