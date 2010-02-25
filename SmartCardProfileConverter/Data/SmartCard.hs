module Data.SmartCard where

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

