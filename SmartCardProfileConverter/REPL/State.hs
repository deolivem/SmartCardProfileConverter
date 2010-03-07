module REPL.State where

import Control.Monad.Trans
import Control.Monad.State
import Data.SmartCard

type REPLState = StateT SmartCard IO 

msgEndProgram = "Copyright Marco De Oliveira 2010"

interactREPL :: (REPLState String -> REPLState (String, Bool)) -> REPLState ()
interactREPL f = do liftIO $ putStr ">"
                    (v, continue) <- f (liftIO $ getLine)
                    liftIO $ putStrLn v
                    if continue
                      then interactREPL f
                      else liftIO $ putStrLn msgEndProgram
