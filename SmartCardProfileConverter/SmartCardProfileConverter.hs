module SmartCardProfileConverter where

import Control.Monad.State
import Data.SmartCard
import REPL
import REPL.State

import System.IO

-- add the state monad
main = do runStateT (interactREPL interactWithCmd) emptySmartCard
          return ()