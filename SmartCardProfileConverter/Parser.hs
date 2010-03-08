module Parser where

import Data.SmartCard (SmartCard)
import Parser.IDF
import System.Directory (doesFileExist)
import System.FilePath (splitExtension)
import Text.ParserCombinators.Parsec


-- Removing comments on the files
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

-- Error display
displayErr :: ParseError -> String
displayErr err = let pos = errorPos err
                   in show (err) ++ " at l. " ++ show (sourceLine pos) ++ " c. " ++ show (sourceColumn pos)


-- return the parser in function of file extention
getParser :: FilePath -> Parser SmartCard
getParser file = let (_, ext) = splitExtension file in
                   case ext of
                     ".idf" -> getSmartCardFromIDF
                     otherwise -> error "File type not supported"

-- File parsing
run p str = case (parse p "" str) of
              (Left err) -> error (displayErr err)
              (Right x)  -> x

runFile :: FilePath -> Parser a -> IO a
runFile file p = do str <- readFile file
                    return $ run p (removeComments str)

parseFile :: FilePath -> IO SmartCard
parseFile file = do exist <- doesFileExist file
                    if (exist)
                      then runFile file (getParser file)
                      else error "File does not exist"
