module SmartCardProfileConverter where

import Text.ParserCombinators.Parsec


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

run p str = case (parse p "" str) of
              (Left err) -> error (displayErr err)
              (Right x)  -> x              


runFile file p withComments = do str <- readFile file
                                 if withComments
                                   then return $ run p str
                                   else return $ run p (removeComments str)

separators = many (oneOf " \n")


getField field = do separators
                    string field
                    separators
                    char '='
                    separators
                    digit

getClockStopMode = getField "ClockStopMode"

getVoltage = getField "Voltage"

getAlgorithmFrequency = getField "AlgorithmFrequency"
