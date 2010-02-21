module SmartCardProfileConverter where

import Text.ParserCombinators.Parsec
import Parser.Tools

displayErr :: ParseError -> String
displayErr err = let pos = errorPos err
                   in show (err) ++ " at l. " ++ show (sourceLine pos) ++ " c. " ++ show (sourceColumn pos)


withoutComments :: Parser String
withoutComments = do first <- many (noneOf "\\/")
                     many (noneOf "\n")
                     next <- remainingWithoutComments
                     return (first ++ next)


remainingWithoutComments :: Parser String
remainingWithoutComments = ( char '\n' >>
                             withoutComments >>=
                             (\value -> return $ '\n':value) )
                           <|> (return [])


removeComments :: String -> String
removeComments str =  case (parse withoutComments "" str) of 
                        (Left err) -> error (displayErr err)
                        (Right x)  -> x


getValue = do separators
              string "Value"
              separators
              bytes

getClockStopMode = getDigitField "ClockStopMode"

getVoltage = getDigitField "Voltage"

getAlgorithmFrequency = getDigitField "AlgorithmFrequency"

getDefine key p = do separators
                     string "Define"
                     separators
                     string key
                     separators
                     value <- p
                     return value

getATR = getDefine "ATR" (many (noneOf "\n"))

getPINContent = do init    <- getBooleanField "Initialised"
                   enable  <- try (getBooleanField "Enabled") <|> return True
                   attempt <- getNumberField "AttemptsRemaining"
                   value   <- getValue
                   return (init, enable, attempt, value)

getPIN1 = getDefine "CHV1" getPINContent

getPUK1 = getDefine "UNBLOCK CHV1" getPINContent

getPIN2 = getDefine "CHV2" getPINContent

getPUK2 = getDefine "UNBLOCK CHV2" getPINContent

-- all field

getProfile = do clockStopMode <- getClockStopMode
                voltage <- getVoltage
                algo <- getAlgorithmFrequency
                atr <- getATR
                pin1 <- getPIN1
                puk1 <- getPUK1
                pin2 <- getPIN2
                puk2 <- getPUK2
                return (clockStopMode, voltage, algo, atr, pin1, puk1, pin2, puk2)


run p str = case (parse p "" str) of
              (Left err) -> error (displayErr err)
              (Right x)  -> x              


runFile file p withComments = do str <- readFile file
                                 if withComments
                                   then return $ run p str
                                   else return $ run p (removeComments str)

                
-- testing commands
test = runFile "example/BTdefault.idf" getProfile False