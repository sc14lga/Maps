{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Maps.Map (
	composeMap,
	region2diagram,
	highlightRegion,
	colorRegion,
	lookupData) where

import MapLoader.SVG
import Maps.Spatial
import Data.List.Split
import qualified Data.Map as Map
import Diagrams.Path 
import Diagrams.Prelude hiding (deep)
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.BrewerSet


-- | PRINTING MODULE -------------------------------------------------------------------------------------------------
-- ...................................................................................................................
-- ___________________________________________________________________________________________________________________

-- |The 'composeMap' method takes all the paths and draws the trail generated in 'drawPaths'. It also colors and styles it
-- BEWARE of the function mconcat and the process of concatenating all the paths/diagrams. There's delicated issues with
-- the positioning
composeMap :: [(String, Int)] -> [Region] -> Diagram B     
composeMap gbp regions = mconcat [ region2diagram gbp region | region <- regions] # scaleY (-1) # lwL 0.2
-- composeMap svg = mconcat [ path | path <- mergeCoords svg] # strokeP # fc purple # scaleY (-1) # lwL 0.2


-- |The 'region2diagram' method takes all the coords of a region and generates a diagram with them
region2diagram :: [(String, Int)] -> Region ->  Diagram B
region2diagram gbp region = highlightRegion (code region) (mconcat [path | path <- coor region] # strokeP # fc ((colorRegion gbp) region) )
--region2diagram region = mconcat [path | path <- coor region] # strokeP # fc purple

highlightRegion :: String -> Diagram B -> Diagram B
highlightRegion "USA" diagram = diagram # lwL 1 # lc red
highlightRegion _ diagram = diagram # lwL 0.2 # lc black


colorRegion :: [(String, Int)] -> Region -> Kolor
colorRegion gbp region = 
	let value = Map.lookup (code region) (Map.fromList $ (scaleToInt gbp 9))
 	in lookupData value

scaleToInt :: [(String, Int)] -> Int -> [(String, Int)] 
scaleToInt array range = [(code, (scale . fromIntegral) value)| (code, value)<-array]
	where
		nums = [value | (_, value)<-array]
		max = (fromIntegral . maximum) nums
		min = (fromIntegral . minimum) nums
		sc = (max - min) / fromIntegral (range - 1)
		scale x = round ((x - min) / sc )

lookupData :: Maybe Int -> Kolor
lookupData Nothing = (brewerSet Greys 9) !! 0 
lookupData (Just something) = (brewerSet YlGnBu 9) !! something

-- | The type area is a Rose Tree ? Contains the set of points and the hierarchy. 
-- And we would like to use it as well as a Hash Table
-- We want to define a group operation
--data Rose a = Region a => Rose a [Rose a] --- ??????

--gbp = [("USA",8),("EMU",8),("EAP",8),("CHN",8),("LCN",8),("NOC",8),("LMC",7),("LAC",7),("JPN",7),("DEU",7),("MEA",7),("GBR",7),("ARB",7),("FRA",7),("SAS",7),("BRA",7),("ITA",7),("IND",7),("RUS",7),("ECA",7),("CAN",7),("SSF",7),("SSA",7),("MNA",6),("CEB",6),("AUS",6),("KOR",6),("ESP",6),("MEX",6),("IDN",5),("NLD",5),("LDC",5),("TUR",5),("SAU",5),("FCS",5),("HPC",5),("SWE",5),("NGA",5),("POL",5),("ARG",5),("BEL",5),("VEN",5),("NOR",5),("AUT",5),("IRN",5),("ARE",5),("LIC",5),("COL",5),("THA",5),("ZAF",4),("DNK",4),("MYS",4),("SGP",4),("ISR",4),("HKG",4),("EGY",4),("PHL",4),("FIN",4),("CHL",4),("PAK",3),("IRL",3),("GRC",3),("PRT",3),("IRQ",3),("DZA",3),("KAZ",3),("QAT",3),("CZE",3),("PER",3),("ROM",3),("VNM",3),("SST",3),("BGD",3),("HUN",3),("UKR",3),("AGO",3),("MAR",3),("ECU",3),("SVK",3),("OSS",3),("OMN",3),("BLR",3),("AZE",3),("LKA",3),("SDN",3),("MMR",3),("DOM",3),("UZB",3),("KEN",3),("GTM",3),("URY",3),("HRV",3),("BGR",2),("MAC",2),("ETH",2),("CRI",2),("SVN",2),("TZA",2),("LTU",2),("TKM",2),("PAN",2),("LBN",2),("SRB",2),("LBY",2),("GHA",2),("JOR",2),("CIV",1),("BOL",1),("BHR",1),("ZAR",1),("CMR",1),("LVA",1),("PRY",1),("ZMB",1),("UGA",1),("EST",1),("SLV",1),("CYP",1),("AFG",1),("NPL",1),("HND",1),("BIH",1),("BRN",1),("GAB",1),("ISL",1),("KHM",1),("GEO",1),("MOZ",1),("BWA",1),("SEN",1),("GNQ",1),("COG",0),("TCD",0),("ZWE",0),("NAM",0),("ALB",0),("SSD",0),("WBG",0),("MUS",0),("BFA",0),("MLI",0),("MNG",0),("NIC",0),("LAO",0),("MKD",0),("ARM",0),("MDG",0),("TJK",0),("BEN",0),("HTI",0),("BHS",0),("PSS",0),("NER",0),("MDA",0),("RWA",0),("KGZ",0),("KSV",0),("GIN",0),("MRT",0),("SLE",0),("MNE",0),("TGO",0),("BRB",0),("MWI",0),("FJI",0),("ERI",0),("SWZ",0),("GUY",0),("BDI",0),("MDV",0),("LSO",0),("LBR",0),("CPV",0),("BTN",0),("CAF",0),("DJI",0),("TMP",0),("SYC",0),("LCA",0),("ATG",0),("SLB",0),("GNB",0),("GRD",0),("KNA",0),("GMB",0),("WSM",0),("VCT",0),("COM",0),("DMA",0),("TON",0),("STP",0),("PLW",0),("KIR",0)]