{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import MapLoader.SVG
import DataLoader.CSV
import Maps.Spatial
import Maps.DataType 
import qualified Maps.Map as M
import Diagrams.Prelude hiding (deep)
import Diagrams.Backend.SVG.CmdLine
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Csv as C

-- | MAIN ------------------------------------------------------------------------------------------------------------
-- ...................................................................................................................
-- ___________________________________________________________________________________________________________________

main :: IO ()
main = do
  csvData <- BL.readFile "source/indicators/CSVs/worldGBPCountries.csv"
  -- In this file we load the classification of countries per continent (File provided by the World Bank)
  hier <- BL.readFile "source/indicators/CSVs/countryClassif.csv"
  continents <- BL.readFile "source/indicators/CSVs/continentId.csv"
  regions <- parseFile "source/worldmap.svg"
  let tree = addHierarchy (parseCSVstruc hier) 1 (buildTree regions)
  let dscont = parseCSV continents 1:: DataSet StvNominal
  -- With the indication of fSub 1 we apply the data to the subset of regions in the level 1.
  let res = M.buildDiagram $ [M.printRegions # M.mapData M.mapHue dscont # M.fSub 1] `M.from` ($ tree)
  mainWith ( res )

