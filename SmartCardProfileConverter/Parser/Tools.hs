module Parser.Tools where

import Text.ParserCombinators.Parsec

-- Basic type

-- parse a space or a endline
separator = oneOf " \n"

-- skip one or more separators
separators = many separator

-- parse a boolean word "Yes" or "No"
boolean = do
          v <- string "Yes" <|> string "No"
          if v == "Yes"
            then return True
            else return False

hexadecimal = do v <- oneOf "0123456789ABCDEF"
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
                   'A' -> return 10
                   'B' -> return 11
                   'C' -> return 12
                   'D' -> return 13
                   'E' -> return 14
                   'F' -> return 15

-- parse a byte represented by an hexadecimal string
byte = do first  <- hexadecimal
          second <- hexadecimal
          return (first*16 + second)

-- parse bytes represented by an hexadecimal string
bytes = do separators
           x  <- byte
           xs <- (try bytes) <|> return []
           return $ x:xs

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

decimals = do separators
              x <- decimal
              y <- try decimals <|> return []
              return (x:y)

number = fmap convert decimals
          where convert [] = 0
                convert xs = (convert (init xs))*10 + last xs
-- Advance structure

-- parse a field of the format "fieldName = fieldValue"
getField fieldName fieldParse = do separators
                                   string fieldName
                                   separators
                                   char '='
                                   separators
                                   fieldParse

-- parse a field containing a digit
getDigitField fieldName = getField fieldName (many digit)

-- parse a field containing a boolean
getBooleanField fieldName = getField fieldName boolean

-- parse a field containing a number
getNumberField fieldName = getField fieldName number