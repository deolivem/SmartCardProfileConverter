module Converter where

import Text.ParserCombinators.Parsec

test = "without comment line\n\\\\with comment line\npartial comment \\\\embedded on the midle of the code"

displayErr :: ParseError -> String
displayErr err = let pos = errorPos err
                   in show (err) ++ " at l. " ++ show (sourceLine pos) ++ " c. " ++ show (sourceColumn pos)


withoutComments :: Parser String
withoutComments = do first <- many (noneOf "\\")
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

separators = many (oneOf " \n")

-- récupération des informations contenus dans le fichier
