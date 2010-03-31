module Main where

import Control.Monad.State
import Control.Monad.Trans
import Data.SmartCard
import Data.SmartCard.Class.Default
import REPL
import REPL.State

import System.IO

msgStartup = "\nSmartCard Profile converter\n\nCommands supported:\ncd:          Change the current directory\nload file:   Load a smartcard profile\nexport file: Export a smartcard profile\nquit:        Quit the application"
msgEnd = "Copyright Marco De Oliveira 2010"

interactREPL :: (REPLState String -> REPLState (String, Bool)) -> REPLState ()
interactREPL f = do liftIO $ putStr "> "
                    (v, continue) <- f (liftIO $ getLine)
                    if continue
                      then (liftIO $ putStrLn v) >> interactREPL f
                      else return()

main :: IO ()
main = do putStrLn msgStartup
          (_, profile) <- runStateT (interactREPL interactWithCmd) defaultValue
          putStrLn msgEnd
          putStrLn $ show profile
          return ()