module REPL where

import Control.Exception (IOException)
import Control.Monad.Trans
import Control.Monad.State
import Parser
import REPL.State
import System.Directory (setCurrentDirectory)
import Text.ParserCombinators.Parsec

data Cmd = CmdChangeDirectory String
         | CmdLoad String
         | CmdExport String
         | CmdQuit
         | CmdHelp
         deriving (Show, Eq)

-- list of the command:
-- cd: change the current directory
-- load: load a file containing the SmartCard Profile
-- export: export the current profile to 

parsingString :: Parser String
parsingString = do char '\"'
                   v <- many1 ( noneOf "\n\"")
                   char '\"'
                   return v

parsingWord :: Parser String
parsingWord = do many1 (noneOf " ")

parsingChangeDirectory :: Parser Cmd
parsingChangeDirectory = do string "cd"
                            space
                            str <- parsingString <|> parsingWord <?> "No directory name"
                            return $ CmdChangeDirectory str

parsingLoad :: Parser Cmd
parsingLoad = do string "load"
                 space
                 str <- parsingWord <|> parsingString <?> "No file name"
                 return $ CmdLoad str
                 
parsingExport :: Parser Cmd
parsingExport = do string "export"
                   space
                   str <- parsingWord <|> parsingString <?> "No file name"
                   return $ CmdExport str

parsingQuit :: Parser Cmd
parsingQuit = do string "quit"
                 return $ CmdQuit

parsingHelp :: Parser Cmd
parsingHelp = do string "help"
                 return $ CmdHelp

parsingCmd :: Parser Cmd
parsingCmd = parsingChangeDirectory <|> parsingLoad <|> parsingExport <|> parsingQuit <|> parsingHelp <?> "Unknown command"

patchDirectoryName :: String -> String
patchDirectoryName (x:xs) = if x == '\\'
                            then "\\\\" ++ patchDirectoryName xs
                            else x : patchDirectoryName xs
patchDirectoryName [] = []
                            
cmdChangeDirectory :: String -> IO (String, Bool)
cmdChangeDirectory dir = let path = patchDirectoryName dir in
                          catch (setCurrentDirectory path >> return ("Directory change...", True))
                               (\e -> return ("Error: " ++ show (e::IOException), True))

evalCmd :: REPLState Cmd -> REPLState (String, Bool)
evalCmd x = do v <- x
               case v of
                 CmdQuit                -> return $ ("Quit the application", False)
                 CmdChangeDirectory dir -> liftIO $ cmdChangeDirectory dir
                 CmdLoad file           -> do sc <- liftIO $ parseFile file 
                                              put sc
                                              return ("File loaded...", True)
                 otherwise    -> return $ (show v, True)

interactWithCmd :: REPLState String -> REPLState (String, Bool)
interactWithCmd strT = do str <- strT
                          case (parse parsingCmd "" str) of
                            (Left err) -> return $ (displayErr err, True)
                            (Right x) -> evalCmd $ return x