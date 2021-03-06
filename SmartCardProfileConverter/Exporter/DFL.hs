module Exporter.DFL where

import Data.Word
import Data.Hexadecimal
import Data.List
import Data.SmartCard
import Data.SmartCard.ATR
import Data.SmartCard.File
import Data.SmartCard.File.Types
import Data.SmartCard.Ki
import Data.SmartCard.PIN



exportToDFL :: SmartCard -> String
exportToDFL sc = intercalate [returnLine] [ showClockStopMode (clockStopMode sc),
                                            showVoltage (voltage sc),
                                            intercalate [returnLine] (map showPIN (security sc)),
                                            intercalate [returnLine] (map showFile (filesystem sc))]

returnLine = '\n'
tab = "  "  -- tabulation are coded with 4 spaces


split :: Eq a => a -> [a] -> [[a]]
split _         []  = []
split separator str = first : (split separator last)
                      where
                        (first, last) = _findToken separator [] str
                        _findToken tok prev []     = (prev, [])
                        _findToken tok prev (x:xs) = if x == tok
                                                       then (prev, xs)
                                                       else _findToken tok (prev++[x]) xs

-- addBorder
addBorder :: Int -> String -> String
addBorder size str = _addBorder size size [] str
                     where
                       _addBorder size currItem previous (x:xs) =
                         if currItem == 0
                           then _addBorder size size (previous ++ [returnLine]) xs
                           else _addBorder size (currItem - 1) (previous ++ [x]) xs
                       _addBorder _ _ previous [] = previous

-- add a  tabulation a character after each return line and the first line
incrementText :: String -> String -> String
incrementText str inc = let strWithoutReturn = split returnLine str 
                            sep = returnLine : inc
                        in inc ++ intercalate sep strWithoutReturn

                        
showBool :: Bool -> String
showBool True  = "Yes"
showBool False = "No"


showFileID :: FileID -> String
showFileID (FileID (x, y)) = toHex x ++ toHex y


showField :: String -> String -> String
showField fieldName fieldValue = fieldName ++ " = " ++ fieldValue ++ [returnLine]


showFields :: [(String, String)] -> String
showFields [] = []
showFields ((fieldName, fieldValue):[]) = showField fieldName fieldValue
showFields ((fieldName, fieldValue):xs) = showField fieldName fieldValue ++ showFields xs


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

showPIN :: PIN -> String
showPIN (PIN str init enable attempts value) =
    showDefine str $ showFields [("Initialised", showBool init), 
                                 ("Disable", (showBool . not) enable),
                                 ("AttemptsRemaining", show attempts)]
                    ++ showValue value
    where
      showValue :: [Word8] -> String
      showValue xs = "Value\n" ++ showW8 xs " "

      
showFile :: File -> String
showFile (MasterFile _) = showMasterFile
showFile (DedicatedFile id parentId) = showDedicatedFile id parentId
showFile (ElementaryFile id parentId inv accWhInv readC updateC incC invC rehC content) = showElementaryFile id parentId inv accWhInv readC updateC incC invC rehC content


showMasterFile :: String
showMasterFile = showDefine "MF DF 3F00" []


showDedicatedFile :: FileID -> FileID -> String
showDedicatedFile fileID parentFileID = showDefine ("DF " ++ showFileID fileID) (showField "Parent" (showFileID parentFileID))


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
                   content =
  let structure = case content of
                    EFTransparent _ -> "Transparent"
                    EFLinearFixed _ -> "LinearFixed"
                    EFCyclic      _ -> "Cyclic"
      showAccessCond cond = case cond of
                              AccessAlways -> "0"
                              AccessPIN    -> "1"
                              AccessPIN2   -> "2"
                              AccessADM    -> "10"
                              AccessNever  -> "15"
   in showDefine ("EF " ++ showFileID fileID) (showFields[("Parent", showFileID parentFileID),
                                                          ("Structure", structure)])
                                              ++ incrementText (showEFContent content) tab
                                              ++ "\n"

showEFContent (EFTransparent content) = "Data\n" ++ (addBorder 47 (showW8 content " "))
showEFContent (EFLinearFixed (x:xs)) = "Record\n" ++ (addBorder 47 (showW8 x " ")) ++ "\n" ++ showEFContent (EFLinearFixed xs)
showEFContent (EFLinearFixed []) = ""
