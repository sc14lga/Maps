{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import MapLoader.SVG
import DataLoader.CSV
import Maps.Spatial
import Maps.DataType 
import qualified Maps.Map as M
import qualified Data.Map as Map
import Diagrams.Prelude hiding (deep)
import Diagrams.Backend.SVG.CmdLine
import qualified Data.ByteString.Lazy as BL
import Data.Colour.Palette.BrewerSet
import qualified Data.Vector as V
import qualified Data.Csv as C

-- | MAIN ------------------------------------------------------------------------------------------------------------
-- ...................................................................................................................
-- ___________________________________________________________________________________________________________________

-- |In this file we will map a composite parameter, which is the division between the population and the net migration with
--  a custom graph over the regions, which is described in the method "customSymbol"
main :: IO ()
main = do
  migrations <- BL.readFile "data/netMigration2013.csv"
  gbpdata <- BL.readFile "source/indicators/CSVs/worldGBPCountries.csv"
  population <- BL.readFile "data/population.csv"
  regions <- parseFile "source/worldmap.svg"
  let tree = buildTree regions
  let changeColors ds@EntitySet{c = ctx}= ds{c = ctx{brewerset = RdYlBu}}
  let m = changeColors (parseCSV migrations 1:: DataSet StvRatio)
  let g = parseCSV gbpdata 1:: DataSet StvRatio
  let p = parseCSV population 1:: DataSet StvRatio
  let gbp = dsToNominal g (\x -> if x>327000000000 then 0 else 1) ["circle","square"]
  let mbyp = compositeDs (\a b -> a / b) m p
  let res = M.buildDiagram $ [customSymbol gbp m p # M.fAll, M.printRegions # M.mapData M.mapHue mbyp # M.fAll] `M.from` ($ tree)
  mainWith ( res )

-- | The custom simbol will map: 
--   - A shape depending on the gbp of the country, being a circle for the rich and a square for the poor
--   - A color depending on the net migrations
--   - A size depending on the population of the country
customSymbol :: (RetHue b, RetSize c) => DataSet String -> DataSet b -> DataSet c ->(SPtree, [Region]) -> [(String, Diagram B)]
customSymbol is ic iz (tree, regions) = 
	let
		face r = (innerShape r) # fc (innerColor r) # scale (innerSize r)
		innerShape code = symbShape $ Map.lookup code (v is)
		innerColor code = hue (c ic) $ Map.lookup code (v ic)
		innerSize code = size' (5,20) (c iz) ( Map.lookup code (v iz) )
		center c = (\(Just p)->p) $ rcenter c regions
		pos c = position [(center c,face c)]
	in [ ( "face - " ++ c, pos c) | region <- regions, let c = code region]

symbShape :: Maybe String -> Diagram B
symbShape Nothing = triangle 1
symbShape (Just "circle") = circle 1
symbShape _ = square 1

