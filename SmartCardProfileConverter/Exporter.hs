module Exporter where

import Exporter.IDF
import Exporter.DFL

getExporter file = let (_, ext) = splitExtension file in
                     case ext of
                       "idf" -> exportToIDF
                       otherwise -> error "file type not supported"