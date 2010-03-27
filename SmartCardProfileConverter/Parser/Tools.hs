module Parser.Tools where

import Data.Hexadecimal
import Text.ParserCombinators.Parsec

-- Basic type

-- parse a space or a endline
separator :: Parser Char
separator = oneOf " \n"

-- skip one or more separators
separators :: Parser String
separators = many separator

-- parse a boolean word "Yes" or "No":i PAr
boolean :: Parser Bool
boolean = do
          v <- string "Yes" <|> string "No"
          if v == "Yes"
            then return True
            else return False

decimal :: Parser Integer
decimal = do v <- oneOf "0123456789"
             case v of
               '0' -> return 0
               '1' -> return 1
               '2' -> return 2
               '3' -> return 3
               '4' -> return 4
               '5' -> return 5
               '6' -> return 6
               '7' -> return 7
               '8' -> return 8
               '9' -> return 9

decimals :: Parser [Integer]
decimals = do separators
              x <- decimal
              y <- try decimals <|> return []
              return (x:y)

number :: Parser Integer
number = fmap convert decimals
          where convert [] = 0
                convert xs = (convert (init xs))*10 + last xs
-- Advance structure

-- parse a field of the format "fieldName = fieldValue"
getField :: String -> Parser a -> Parser a
getField fieldName fieldParse = do separators
                                   string fieldName
                                   separators
                                   char '='
                                   separators
                                   fieldParse

-- Parse a field of the format "fieldName fieldValue" (without '=')
getField2 :: String -> Parser a -> Parser a
getField2 fieldName fieldParse = do separators
                                    string fieldName
                                    separators
                                    fieldParse
                                   
-- parse a field containing a digit
getDigitField fieldName = getField fieldName (many digit)

-- parse a field containing a boolean
getBooleanField fieldName = getField fieldName boolean

-- parse a field containing a number
getNumberField fieldName = getField fieldName number

-- parse a field containing AlphaNum caracter
getStringField fieldName = getField fieldName (many1 alphaNum)



parseDefine :: Parser (Parser a) -> Parser a
parseDefine parseKey = do separators
                          string "Define"
                          separators
                          parser <- parseKey
                          separators
                          value <- parser
                          return value


data FileStructure = Transparent
                   | LinearFixed
                   | Cyclic
                   deriving (Show, Eq)

transparent :: Parser FileStructure
transparent = do string "Transparent"
                 return Transparent

linearFixed :: Parser FileStructure
linearFixed = do string "LinearFixed"
                 return LinearFixed

cyclic :: Parser FileStructure
cyclic = do string "Cyclic"
            return Cyclic

structure :: Parser FileStructure
structure = try (transparent) <|> try (linearFixed) <|> try (cyclic) <?> "Unknown file structure"

  