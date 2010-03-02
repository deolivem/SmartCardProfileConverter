module Data.SmartCard.File.Types where

import Data.Word

data FileID = FileID (Word8, Word8)
            deriving (Show)

convFileID (x:y:xs) = FileID (x, y)
convFileID _ = error "Not a valid FileID"

data SFID = SFID Word8
          deriving (Show)
data EFAccessCondition = AccessAlways
                       | AccessPIN
                       | AccessPIN2
                       | AccessADM
                       | AccessNever
                       deriving (Show, Eq)

convAccessCondition a = case a of 
                          0 -> AccessAlways
                          1 -> AccessPIN
                          2 -> AccessPIN2
                          10 -> AccessADM
                          11 -> AccessADM
                          15 -> AccessNever
                          otherwise -> error "Invalide Access condition"
                       
data EFContent = EFTransparent [Word8]
               | EFLinearFixed [[Word8]]
               | EFCyclic [[Word8]]
               deriving (Show)