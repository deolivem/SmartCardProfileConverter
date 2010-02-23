module Parser.ParseIDF where

import Data.Word
import Parser.Tools
import Text.ParserCombinators.Parsec

-- hardware description
getClockStopMode = getDigitField "ClockStopMode"
getVoltage = getDigitField "Voltage"
getAlgorithmFrequency = getDigitField "AlgorithmFrequency"

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
                   return $ DefineATR value

_getPINContent name = do init    <- getBooleanField "Initialised"
                         enable  <- try (getBooleanField "Enabled") <|> return True
                         attempt <- getNumberField "AttemptsRemaining"
                         value   <- getValue
                         return $ (DefineCHV name init enable attempt value)

getKiContent = do value <- bytes 
                  return $ DefineKi value

getMFContent = do separators
                  string "DF"
                  separators
                  value <- bytes
                  return $ DefineMS value

getDFContent = do value <- bytes
                  parent <- getBytesField "Parent"
                  return $ DefineDF value parent

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
                            return $ DefineEF value parent invalidated accessWhenInvalid struct read update increase invalidate rehabilitate [xs]
                    else do
                            xss <- getRecord
                            return $ DefineEF value parent invalidated accessWhenInvalid struct read update increase invalidate rehabilitate xss

parsePIN = do name <- try (string "CHV1") <|> try (string "UNBLOCK CHV1") <|> try (string "CHV2") <|> string "UNBLOCK CHV2"
              return $ _getPINContent name

parseATR = do string "ATR"
              return $ getATRContent

parseKi = do string "Ki"
             return $ getKiContent

parseMF = do string "MF"
             return $ getMFContent

parseDF = do string "DF"
             return $ getDFContent

parseEF = do string "EF"
             return $ getEFContent
             
parseDefineKey = try parseATR
             <|> try parsePIN
             <|> try parseKi
             <|> try parseMF
             <|> try parseDF
             <|> try parseEF
             <?> "Unknow define"
define = do separators
            string "Define"
            separators
            parser <- parseDefineKey
            separators
            value <- parser
            return value