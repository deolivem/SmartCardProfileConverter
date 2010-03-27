module Parser.IDF (getSmartCardFromIDF) where

import Data.Hexadecimal
import Data.SmartCard
import Data.SmartCard.ATR
import Data.SmartCard.File
import Data.SmartCard.File.Types
import Data.SmartCard.Ki
import Data.SmartCard.PIN
import Data.Word
import Parser.Tools
import Text.ParserCombinators.Parsec

-- String parsing for hexadecimal value

-- In DFL file, hexadecimal value are coded with big letter
hexadecimal :: Parser Char
hexadecimal = do oneOf "0123456789ABCDEF"

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
               
-- card contents
getATRContent :: Parser ATR
getATRContent = do value <- bytes
                   return $ ATR value

getKiContent :: Parser Ki
getKiContent = do value <- bytes 
                  return $ Ki value

getPINContent :: String -> Parser PIN
getPINContent name = do init    <- getBooleanField "Initialised"
                        enable  <- try (getBooleanField "Enabled") <|> return True
                        attempt <- getNumberField "AttemptsRemaining"
                        value   <- getValue
                        return $ (PIN name init enable attempt value)

getMFContent :: Parser File
getMFContent = do separators
                  string "DF"
                  separators
                  value <- bytes
                  return $ MasterFile (convFileID value)

getDFContent :: Parser File
getDFContent = do value <- bytes
                  parent <- getBytesField "Parent"
                  return $ DedicatedFile (convFileID value) (convFileID parent)

getEFContent :: Parser File
getEFContent = do value             <- bytes
                  parent            <- getBytesField "Parent"
                  invalidated       <- getBooleanField "Invalidated"
                  accessWhenInvalid <- getBooleanField "AccessibleWhenInvalidated"
                  struct            <- getField "Structure" structure
                  read              <- try (getNumberField "ReadPolicy") <|> return 1
                  update            <- try (getNumberField "UpdatePolicy") <|> return 1
                  increase          <- try (getNumberField "IncreasePolicy") <|> return 1
                  invalidate        <- try (getNumberField "InvalidatePolicy") <|> return 1
                  rehabilitate      <- try (getNumberField "RehabilitatePolicy") <|> return 1
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

parseATR :: Parser (Parser ATR)
parseATR = do string "ATR"
              return $ getATRContent

parseKi :: Parser (Parser Ki)
parseKi = do string "Ki"
             return $ getKiContent

parseMF :: Parser (Parser File)
parseMF = do string "MF"
             return $ getMFContent

parseDF :: Parser (Parser File)
parseDF = do string "DF"
             return $ getDFContent

parseEF :: Parser (Parser File)
parseEF = do string "EF"
             return $ getEFContent

parseFile :: Parser (Parser File)
parseFile = try parseEF
        <|> try parseDF
        <|> try parseMF
        <?> "Unknown File"

getATR :: Parser ATR
getATR = parseDefine parseATR

getKi :: Parser Ki
getKi = parseDefine parseKi

getPIN :: Parser PIN
getPIN = parseDefine parsePIN

getFile :: Parser File
getFile = parseDefine parseFile

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

getAlgorithmFrequency :: Parser Frequency
getAlgorithmFrequency = do value <- getDigitField "AlgorithmFrequency"
                           case value of
                             "4" -> return $ FourMHertz
                             "8" -> return $ EightMHertz
                             otherwise -> fail "Unknown value for AlgorithmFrequency"

getSmartCardFromIDF :: Parser SmartCard
getSmartCardFromIDF = do clockStopMode <- getClockStopMode
                         voltage <- getVoltage
                         frequency <- getAlgorithmFrequency
                         atr <- getATR
                         security <- many1 $ try getPIN
                         ki <- getKi
                         filesystem <- many1 $ try getFile
                         return $ SmartCard voltage clockStopMode frequency atr ki security filesystem