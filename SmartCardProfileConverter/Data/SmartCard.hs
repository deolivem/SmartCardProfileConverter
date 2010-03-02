module Data.SmartCard where

import Data.SmartCard.ATR
import Data.SmartCard.File
import Data.SmartCard.Ki
import Data.SmartCard.PIN



-- definition of the different parameters
data ClockStopMode = ClockStopNotAllowed
                   | ClockStopNotAllowedUnlessAtLowLevel
                   | ClockStopNotAllowedUnlessAtHighLevel
                   | ClockStopAllowedNoPreferredLevel
                   | ClockStopAllowedLowLevelPreferred
                   | ClockStopAllowedHighLevelPreferred
                   deriving (Show)

data Voltage = FiveVolt
             | ThreeVolt
             | OneDotEightVolt
             deriving (Show)

data Frequency = FourMHertz
               | EightMHertz
               deriving (Show)
             
data SmartCard =
  SmartCard { voltage :: Voltage,
              clockStopMode :: ClockStopMode,
              frequency :: Frequency,
              atr :: ATR,
              ki :: Ki,
              security :: [PIN],
              filesystem :: [File]}
              deriving (Show)