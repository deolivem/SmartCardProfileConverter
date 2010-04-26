module Exporter where

import Data.SmartCard
import Exporter.IDF
import Exporter.DFL
import System.FilePath
import System.IO

getExporter :: FilePath -> SmartCard -> String
getExporter file = let (_, ext) = splitExtension file in
                     case ext of
                       ".idf" -> exportToIDF
                       otherwise -> error "file type not supported"


exportSmartCardToFile :: SmartCard -> FilePath -> IO ()
exportSmartCardToFile sc file = writeFile file (getExporter file sc) 
