{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Maps.Map (
	buildDiagram,
	fAll,
	fSub,
	Maps.Map.from,
	printRegions,
	printArrows,
	mapData,
	mapHue,
	mapLightless) where

import Maps.Spatial
import qualified Maps.DataType as T
import qualified Data.Map as Map
import qualified Data.Sequence as S
import Diagrams.Prelude hiding (deep)
import Diagrams.Backend.SVG.CmdLine


-- | MAPS MODULE -----------------------------------------------------------------------------------------------------
-- ...................................................................................................................
-- ___________________________________________________________________________________________________________________

-- |Synonim DiagramList to mantain the code more legible.
type RegDiagram = (String, Diagram B)
type DiagramList = [RegDiagram]
type Regions = (SPtree, [Region])

-- | buildDiagram : Symply merges a list of diagrams in absolute positions and generates a resulting composition.
buildDiagram :: [DiagramList] -> Diagram B
buildDiagram diagrams = mconcat [ diagram | (code, diagram) <- concat diagrams] # scaleY (-1) # lwL 0.2

-- | from : This function receives a list of functions and maps it to a tree, to produce a diagram list that will be 
--   used in buildDiagram
from :: [SPtree -> DiagramList] -> ((SPtree -> DiagramList) -> DiagramList) -> [DiagramList]
from = flip map

-- | fAll : This function receives a function and a tree, and maps the function to the lower level (Level of detail 0) of the tree
fAll :: (Regions -> DiagramList) -> SPtree -> DiagramList
fAll f tree = f (tree, mostSpecific tree)

-- | fSub : This function receives a function and a tree, and maps the function to the desired level (int param) of the tree
fSub :: Int -> (Regions -> DiagramList) -> SPtree -> DiagramList
fSub i f tree = f (tree, [v | (i, v) <- (S.index tree i)])

-- | mapData : This function receives:
--   1- A function that applies data to a diagram and returns it updated
--   2- A dataset with the data to apply
--   3- A function that generates diagrams from regions
--   4- The regions
--  The function generates the diagrams with the regions(4) and the functions(3), and to every diagram created applies the
--  transforming function (1) with the data (2)

mapData :: (T.DataSet a -> RegDiagram -> RegDiagram) -> T.DataSet a -> (Regions -> DiagramList) -> Regions -> DiagramList
mapData func list f d = [ func list diagram | diagram <- (f d )]

-- | mapHue : Applies a hue to a diagram
mapHue ::(T.RetHue a) => T.DataSet a -> RegDiagram -> RegDiagram 
mapHue ds (code, dia) = let 
		value = Map.lookup (code) (T.v ds)
		color = T.hue (T.c ds) value
	in ( code, dia # fc color )

-- | mapLightless : Applies ligntless to a diagram
mapLightless ::(T.RetLightless a) => T.DataSet a -> RegDiagram -> RegDiagram
mapLightless ds (code, dia) = let 
		value = Map.lookup (code) (T.v ds)
		lightless = T.lightless (T.c ds) value
	in ( code, dia # opacity lightless )

-- | printRegions : Generates a list of Diagrams from a selection of regions
printRegions :: Regions -> DiagramList
printRegions (tree, regions) = 
	let region2map region = mconcat (area region tree) # strokeP # named (code region)
	in  [ ( code region, region2map region) | region <- regions]

-- | printArrows : Generates a list of arrows from a dataset and a 
printArrows :: (T.RetSize a) => T.DataSet a ->  Regions -> DiagramList
printArrows T.RelationSet{T.rv = list, T.c = ctx} (tree, regions) = 
	let
		pt p  = rcenter p regions
		siz val = T.size' (0.2,1) ctx (Just val)
		point p = (\(Just p) -> p) (pt p)
		arrow c1 c2 size= arrowBetween' (styleArrow size) (point c1) (point c2)
	in  [  ("--", arrow c1 c2 (siz val))| record@(c1, c2, val) <- list, pt c1/=Nothing, pt c2/=Nothing]
printArrows _ _ = []

-- | styleArrow : Styles the arrows
styleArrow :: Double -> ArrowOpts Double 
styleArrow size = let
		sh = arc xDir (1/16 @@ turn)
		gradient = mkLinearGradient stops ((-1) ^& (-1)) (1 ^& 1) GradPad
		stops = mkStops [(green, 0, 1), (red, 1, 1)]
	in (with & headLength .~ (tiny/4) 	& arrowShaft .~ sh
			& headGap .~ tiny 			& tailLength .~ (tiny/4) 
			& arrowTail .~ quill 		& shaftTexture .~ gradient 
			& shaftStyle %~ lwO size )

-- | scaleToInt : Utility function to scale to int
scaleToInt :: [(String, Int)] -> Int -> [(String, Int)] 
scaleToInt array range = [(code, (scale . fromIntegral) value)| (code, value)<-array]
	where
		nums = [value | (_, value)<-array]
		max = (fromIntegral . maximum) nums
		min = (fromIntegral . minimum) nums
		sc = (max - min) / fromIntegral (range - 1)
		scale x = round ((x - min) / sc )