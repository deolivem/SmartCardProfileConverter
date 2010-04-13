module Exporter.IDF where

import Data.Word
import Data.Hexadecimal
import Data.List
import Data.SmartCard
import Data.SmartCard.ATR
import Data.SmartCard.File
import Data.SmartCard.File.Types
import Data.SmartCard.PIN



exportToIDF _ = ""


-- Definition of characters
returnLine = '\n'
tab = "    "  -- tabulation are coded with 4 spaces


-- squelette de construction de chaîne de charactères

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


showFields :: [(String, String)] -> String
showFields [] = []
showFields ((fieldName, fieldValue):[]) = showField fieldName fieldValue
showFields ((fieldName, fieldValue):xs) = showField fieldName fieldValue ++ "\n" ++ showFields xs


showDefine :: String -> String -> String
showDefine title content = "Define "
                         ++ title
                         ++ [returnLine, returnLine]
                         ++ (incrementText content tab)
                         ++ [returnLine]


-- construction de chaîne de charactère pour des types prédéfinis

-- Display a 
showW8 :: [Word8] -> String -> String
showW8 []     _   = ""
showW8 (x:[]) _   = toHex x
showW8 (x:xs) sep = toHex x ++ sep ++ showW8 xs sep


showBool :: Bool -> String
showBool True  = "Yes"
showBool False = "No"


showValue :: [Word8] -> String
showValue xs = "Value\n" ++ showW8 xs " "


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


showPIN :: PIN -> String
showPIN (PIN str init enable attempts value) =
    showDefine str $ showFields [("Initialised", showBool init), 
                                 ("Enabled", showBool enable),
                                 ("AttemptsRemaining", show attempts)]
                    ++ showValue value

                    
showMasterFile :: String
showMasterFile = showDefine "MF DF 3F00" []


showDedicatedFile :: FileID -> FileID -> String
showDedicatedFile fileID parentFileID = showDefine ("DF " ++ show fileID) (showField "Parent" (show parentFileID))


-- ElementaryFile FileID FileID Bool Bool EFAccessCondition EFAccessCondition EFAccessCondition EFAccessCondition EFAccessCondition EFContent
showElementaryFile :: FileID
                   -> FileID
                   -> Bool
                   -> Bool
                   -> EFAccessCondition
                   -> EFAccessCondition
                   -> EFAccessCondition
                   -> EFAccessCondition
                   -> EFAccessCondition
                   -> EFContent
                   -> String
showElementaryFile fileID
                   parentFileID
                   invalid
                   accessWhenInv
                   readAccessCond
                   updateAccessCond
                   increaseAccessCond
                   invalidateAccessCond
                   rehabilitateAccessCond
                   content = let structure = case content of
                                               EFTransparent _ -> "Transparent"
                                               EFLinearFixed _ -> "LinearFixed"
                                               EFCyclic      _ -> "Cyclic"
                                 showAccessCond cond = case cond of
                                                         AccessAlways -> "0"
                                                         AccessPIN    -> "1"
                                                         AccessPIN2   -> "2"
                                                         AccessADM    -> "10"
                                                         AccessNever  -> "15"
                                -- showContent = 
                             in showDefine ("EF " ++ show fileID) (showFields[("Parent", show parentFileID),
                                                                              ("Invalidated", showBool invalid),
                                                                              ("AccessibleWhenInvalidated", showBool accessWhenInv),
                                                                              ("Structure", structure),
                                                                              ("ReadPolicy", showAccessCond readAccessCond),
                                                                              ("UpdatePolicy", showAccessCond updateAccessCond),
                                                                              ("IncreasePolicy", showAccessCond increaseAccessCond),
                                                                              ("InvalidatePolicy", showAccessCond invalidateAccessCond), 
                                                                              ("RehabilitatePolicy", showAccessCond rehabilitateAccessCond)])