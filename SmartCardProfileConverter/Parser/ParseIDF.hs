module Parser.ParseIDF where

import Data.Word
import Data.SmartCard
import Data.SmartCard.ATR
import Data.SmartCard.File
import Data.SmartCard.File.Types
import Data.SmartCard.Ki
import Data.SmartCard.PIN
import Parser.Tools
import Text.ParserCombinators.Parsec

-- card contents

data FileStructure = Transparent
                   | LinearFixed
                   | Cyclic
                   deriving (Show, Eq)

transparent = do string "Transparent"
                 return Transparent

linearFixed = do string "LinearFixed"
                 return LinearFixed
                 
cyclic = do string "Cyclic"
            return Cyclic

structure = try (transparent) <|> try (linearFixed) <|> try (cyclic) <?> "Unknown file structure"

getData = do separators
             string "Data"
             separators
             bytes

getRecord = do separators 
               string "Record"
               separators
               x <- bytes
               xs <- try (getRecord) <|> return []
               return (x:xs)

data IDFDefine = DefineATR [Word8]
               | DefineCHV String Bool Bool Integer [Word8]
               | DefineKi [Word8]
               | DefineMS [Word8]
               | DefineDF [Word8] [Word8]
               | DefineEF [Word8] [Word8] Bool Bool FileStructure Integer Integer Integer Integer Integer [[Word8]]

instance Show IDFDefine where
  show (DefineATR xs) = "ATR: " ++ show xs ++ "\n"
  show (DefineCHV n _ _ _ v) = "CHV: " ++ show n ++ " " ++ show v ++ "\n"
  show (DefineKi xs) = "Ki: " ++ show xs ++ "\n"
  show (DefineMS xs) = "MS " ++ show xs ++ "\n"
  show (DefineDF xs ys) = "DF " ++ show xs ++ " Parent: " ++ show ys ++ "\n"
  show (DefineEF xs ys _ _ _ _ _ _ _ _ _) = "EF " ++ show xs ++ " Parent: " ++ show ys ++ "\n"

getATRContent = do value <- bytes
                   return $ ATR value

getPINContent name = do init    <- getBooleanField "Initialised"
                        enable  <- try (getBooleanField "Enabled") <|> return True
                        attempt <- getNumberField "AttemptsRemaining"
                        value   <- getValue
                        return $ (PIN name init enable attempt value)

getKiContent = do value <- bytes 
                  return $ Ki value

getMFContent = do separators
                  string "DF"
                  separators
                  value <- bytes
                  return $ MasterFile (convFileID value)

getDFContent = do value <- bytes
                  parent <- getBytesField "Parent"
                  return $ DedicatedFile (convFileID value) (convFileID parent)

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

parsePIN = do name <- try (string "CHV1") <|> try (string "UNBLOCK CHV1") <|> try (string "CHV2") <|> string "UNBLOCK CHV2"
              return $ getPINContent name

parseATR = do string "ATR"
              return $ getATRContent

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
  
parseDefine :: Parser (Parser a) -> Parser a
parseDefine parseKey = do separators
                          string "Define"
                          separators
                          parser <- parseKey
                          separators
                          value <- parser
                          return value

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

getSmartCard :: Parser SmartCard
getSmartCard = do clockStopMode <- getClockStopMode
                  voltage <- getVoltage
                  frequency <- getAlgorithmFrequency
                  atr <- getATR
                  security <- many1 $ try getPIN
                  ki <- getKi
                  filesystem <- many1 $ try getFile
                  return $ SmartCard voltage clockStopMode frequency atr ki security filesystem