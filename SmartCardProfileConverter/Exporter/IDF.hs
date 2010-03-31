module Exporter.IDF where

import Data.Word
import Data.Hexadecimal
import Data.List
import Data.SmartCard
import Data.SmartCard.ATR


exportToIDF _ = ""

-- Definition of characters
returnLine = '\n'
tab = "    "  -- tabulation are coded with 4 spaces


-- Split a string using a separator
-- example "abvd-123-fdre" with '-' as separator return ["abvd", "123", "fdre"]
split :: Eq a => a -> [a] -> [[a]]
split _         []  = []
split separator str = first : (split separator last)
                      where
                        (first, last) = _findToken separator [] str
                        _findToken tok prev []     = (prev, [])
                        _findToken tok prev (x:xs) = if x == tok
                                                       then (prev, xs)
                                                       else _findToken tok (prev++[x]) xs


-- add a  tabulation a character after each return line and the first line
incrementText :: String -> String -> String
incrementText str inc = let strWithoutReturn = split returnLine str 
                            sep = returnLine : inc
                        in inc ++ intercalate sep strWithoutReturn


showField :: String -> String -> String
showField fieldName fieldValue = fieldName ++ " = " ++ fieldValue ++ [returnLine]


showDefine :: String -> String -> String
showDefine title content = "Define "
                         ++ title
                         ++ [returnLine, returnLine]
                         ++ (incrementText content tab)
                         ++ [returnLine]


showW8 :: [Word8] -> String -> String
showW8 []     _   = ""
showW8 (x:[]) _   = toHex x
showW8 (x:xs) sep = toHex x ++ sep ++ showW8 xs sep


showClockStopMode :: ClockStopMode -> String
showClockStopMode stopMode = showField "ClockStopMode" strStopMode
    where strStopMode = case stopMode of
                          ClockStopNotAllowed                  -> "0"
                          ClockStopNotAllowedUnlessAtLowLevel  -> "1"
                          ClockStopNotAllowedUnlessAtHighLevel -> "2"
                          ClockStopAllowedNoPreferredLevel     -> "4"
                          ClockStopAllowedLowLevelPreferred    -> "5"
                          ClockStopAllowedHighLevelPreferred   -> "6"


showVoltage :: Voltage -> String
showVoltage voltage = showField "Voltage" strVoltage
                      where strVoltage = case voltage of
                                           FiveVolt        -> "5"
                                           ThreeVolt       -> "3"
                                           OneDotEightVolt -> "1.8"


showFrequency :: Frequency -> String
showFrequency frequency = showField "AlgorithmFrequency" strFrequency
                          where strFrequency = case frequency of
                                                 FourMHertz  -> "4"
                                                 EightMHertz -> "8"


showATR :: ATR -> String
showATR (ATR xs) = showDefine "ATR" (showW8 xs " ")
