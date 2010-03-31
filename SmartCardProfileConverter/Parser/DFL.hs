module Parser.DFL (getSmartCardFromDFL) where

import Data.Hexadecimal
import Data.SmartCard
import Data.SmartCard.Class.Default
import Data.SmartCard.File
import Data.SmartCard.File.Types
import Data.SmartCard.PIN
import Data.Word
import Parser.Tools
import Text.ParserCombinators.Parsec

-- String parsing for hexadecimal value

-- In DFL file, hexadecimal value are coded with small letter
hexadecimal :: Parser Char
hexadecimal = do oneOf "0123456789abcdef"

-- Parse a byte represented by an hexadecimal string
byte :: Parser Word8
byte = do first <- hexadecimal
          second <- hexadecimal
          return $ readHex (first:second:[])

-- Parser bytes represented by an hexadecimal string
bytes :: Parser [Word8]
bytes = do separators
           x <- byte
           xs <- (try bytes) <|> return []
           return $ x:xs

hexaDigit :: Parser Integer
hexaDigit = do separators
               x <- hexadecimal
               return $ readHexaChar x

-- parse a fied containing an hexadecimal character
getHexaField fieldName = getField fieldName hexaDigit

-- parse a field containing bytes
getBytesField fieldName = getField fieldName bytes

getBytesField2 fieldName = getField2 fieldName bytes

getValue = do separators
              string "Value"
              separators
              bytes

getData :: Parser [Word8]
getData = do separators
             string "Data"
             separators
             bytes

getRecord :: Parser [[Word8]]
getRecord = do separators 
               string "Record"
               separators
               x <- bytes
               xs <- try (getRecord) <|> return []
               return (x:xs)
               
getPINContent :: String -> Parser PIN
getPINContent name = do init    <- getBooleanField "Initialised"
                        enable  <- fmap not $ try (getBooleanField "Disabled")
                        attempt <- getHexaField "AttemptsRemaining"
                        value   <- getBytesField2 "Value"
                        return $ (PIN name init enable attempt value)

getMFContent :: Parser File
getMFContent = do separators
                  string "Parent"
                  separators
                  bytes
                  return $ MasterFile (convFileID [0x3f, 0x00])

getDFContent :: Parser File
getDFContent = do value <- bytes
                  parent <- getBytesField2 "Parent"
                  return $ DedicatedFile (convFileID value) (convFileID parent)

getEFContent :: Parser File
getEFContent = do value             <- bytes
                  parent            <- getBytesField2 "Parent"
                  invalidated       <- return False  -- this field is not present on DFL files
                  accessWhenInvalid <- return False  -- this field is not present on DFL files
                  struct            <- getField "Structure" structure
                  read              <- return 1      -- this field is not present on DFL files
                  update            <- return 1      -- this field is not present on DFL files
                  increase          <- return 1      -- this field is not present on DFL files
                  invalidate        <- return 1      -- this field is not present on DFL files
                  rehabilitate      <- return 1      -- this field is not present on DFL files
                  if struct == Transparent
                    then do
                            xs <- getData
                            return $ ElementaryFile (convFileID value) (convFileID parent) invalidated accessWhenInvalid (convAccessCondition read) (convAccessCondition update) (convAccessCondition increase) (convAccessCondition invalidate) (convAccessCondition rehabilitate) (EFTransparent xs)
                    else do
                            xss <- getRecord
                            return $ ElementaryFile (convFileID value) (convFileID parent) invalidated accessWhenInvalid (convAccessCondition read) (convAccessCondition update) (convAccessCondition increase) (convAccessCondition invalidate) (convAccessCondition rehabilitate) (EFLinearFixed xss)

parsePIN :: Parser (Parser PIN)
parsePIN = do name <- try (string "CHV1") <|> try (string "UNBLOCK CHV1") <|> try (string "CHV2") <|> string "UNBLOCK CHV2"
              return $ getPINContent name

parseMF :: Parser (Parser File)
parseMF = do string "DF 3F00"
             return $ getMFContent

parseDF :: Parser (Parser File) 
parseDF = do string "DF"
             return $ getDFContent

parseEF :: Parser (Parser File)
parseEF = do string "EF"
             return $ getEFContent

parseFile :: Parser (Parser File)
parseFile = try parseMF
        <|> try parseDF
        <|> try parseEF
        <?> "Unknown file"

getClockStopMode :: Parser ClockStopMode
getClockStopMode = do value <- getDigitField "ClockStopMode"
                      case value of
                        "0" -> return $ ClockStopNotAllowed
                        "1" -> return $ ClockStopNotAllowedUnlessAtLowLevel
                        "2" -> return $ ClockStopNotAllowedUnlessAtHighLevel
                        "3" -> return $ ClockStopAllowedNoPreferredLevel
                        "4" -> return $ ClockStopAllowedLowLevelPreferred
                        "5" -> return $ ClockStopAllowedHighLevelPreferred
                        otherwise -> fail "Unknown value for ClockModeStop"


getVoltage :: Parser Voltage
getVoltage = do value <- getDigitField "Voltage"
                case value of
                  "5"   -> return $ FiveVolt
                  "3"   -> return $ ThreeVolt
                  "1.8" -> return $ OneDotEightVolt
                  otherwise -> fail "Unknown value for voltage"

getPIN :: Parser PIN
getPIN = parseDefine parsePIN

getFile :: Parser File
getFile = parseDefine parseFile
                  
getSmartCardFromDFL :: Parser SmartCard
getSmartCardFromDFL = do clockStopMode <- getClockStopMode
                         voltage <- getVoltage
                         many $ try getVoltage
                         security <- many1 $ try getPIN
                         filesystem <- many1 $ try getFile
                         return $ SmartCard voltage clockStopMode defaultValue defaultValue defaultValue security filesystem