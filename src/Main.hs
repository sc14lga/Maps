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

-- |The 'main' method reads an SVG file, process it through 'generateRegions' and prints it with the 'composeMap function'.
--main = do
main :: IO ()
main = do
  migrations <- BL.readFile "data/netMigration2013.csv"
  population <- BL.readFile "data/population.csv"
  flows <- BL.readFile "data/BilateralMigration2013.csv"
  regions <- parseFile "source/worldmap.svg"
  let tree = buildTree regions
  let m = parseCSV migrations 1:: DataSet StvRatio
  let p = parseCSV population 1:: DataSet StvRatio
  let f = parseCSV flows 2:: DataSet StvRatio
  let mbyp = compositeDs (\a b -> a / b) m p
  let res = M.buildDiagram $ [M.printArrows f # M.fAll, M.printRegions # M.mapData M.mapHue mbyp # M.fAll] `M.from` ($ tree)
  mainWith ( res )
