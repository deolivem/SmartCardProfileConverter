module REPL.State where

import Control.Monad.Trans
import Control.Monad.State
import Data.SmartCard

type REPLState = StateT SmartCard IO 

