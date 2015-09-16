{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Maps.Spatial (
	Region(..),
	SPtree,
	rcenter,
	buildTree,
	mostSpecific,
	addHierarchy,
	area,
	Maps.Spatial.lookup) where

import Diagrams.Path 
import Diagrams.Prelude hiding (deep)
import Data.Foldable (toList)
import qualified Data.List as L
import qualified Data.Sequence as S

-- | SPATIAL MODULE --------------------------------------------------------------------------------------------------
-- ...................................................................................................................
-- ___________________________________________________________________________________________________________________

type Code = String

-- |The 'Region' is an Algebraic Data Type that contains the class or identifier of a shape and a list of Diagram's 
--  paths with its points. A path is a set of located trails.
data Region = Base { code :: Code, coor :: [Path V2 Double]} 
			| Group {code :: Code, level::Int, nested :: [(String, Region)], ids :: [String]} deriving (Eq, Show)

-- |The synonim SPtree represents the spatial structure. It is a sequence of lists, every item of the sequence
--  corresponds to one level of detail (the fact that it is indexed motivates the use of a sequence instead of a list).
--  Inside every level we have a list of tuples with the regions belonging to that level, and their codes.
--  The base level is always the first (index 0), and the rest are constructed over it
--
-- @
-- Level 0: Spain | Portugal | Germany | ...
--            \     /         /
-- Level 1:  Iberian Pen     /
--                 \        /  ...
-- Level 2:          Europe
-- @
type SPtree = S.Seq ([(Code, Region)])

-- |buildTree : Initializes a tree with a single level and the provided regions in the floor.
buildTree :: [Region] -> SPtree
buildTree regions = 
	let list = [(c, reg)|reg<-regions, let c = code reg]
	in S.fromList (list:[])

-- |mostSpecific : Retrieves the regions from the most specific level of detail, that means, level 0
mostSpecific :: SPtree -> [Region]
mostSpecific tree = [v | (i, v) <- S.index tree 0]

-- |rcenter : Calculates the center of a group of regions satisfying a code.
rcenter :: Code -> [Region] -> Maybe (P2 Double)
rcenter cod regions = 
	let
		zone = [ r | r <- regions, code r == cod]
		getSize c = (length . concat . pathPoints . head) c
		ordZone = L.sortBy sortGT zone
		list = [(pathCentroid . head . coor) p | p <- ordZone]
		cntr = if null list then Nothing else Just (head list)
	in cntr

-- |lookup : Returns the region of the code provided or nothing if it's not in the tree.
lookup :: Code -> SPtree -> Maybe Region
lookup cod tree = 
	let 
		fnd list = L.find (\(c,v)->c==cod) list
		rl = [ fnd list | list <- toList tree, fnd list /= Nothing]
		reg = (\(Just (c, r))->Just r) (head rl)
	in if null rl then Nothing else reg 

sortGT reg1 reg2 
  | getSize reg1 < getSize reg2 = GT
  | otherwise = LT
  where getSize = (length . concat . pathPoints . head . coor)

-- |area : Returns the area of a region, which is that contained within the points of coor for a Base region 
--  or the accumulation of the areas of the nested Regions. This is a recursive function.
area:: Region -> SPtree -> [Path V2 Double]
area Base {coor = c} _ = c
area Group {level = l, nested = n} tree = concat [ area reg tree | (c, reg) <- n]

-- |addHierarchy : Loads a complete hierarchy, specified through tuples of (region, list of nested regions)
addHierarchy:: [(String, [String])] -> Int -> SPtree -> SPtree
addHierarchy struc level tree = foldl (\ tree (code, list)-> groupRegions code level list tree) tree struc

-- |groupRegions : Provided a new code and the level where to place it the method:
--  1-Creates the level if it didn't existed yet.
--  2-Creates the region specified with this code and with the list of nested elements provided. 
groupRegions:: Code -> Int -> [Code] -> SPtree -> SPtree
groupRegions newCode level nested tree
  | S.length tree <= level = groupRegions newCode level nested biggerTree
  | otherwise = S.update level newValue tree 
  where 
  		biggerTree = tree |> []
  		newValue = (newCode, Group{code=newCode, level=level, nested=newNested, ids=nested}):(S.index tree level)
  		newNested = getRegions nested tree level

-- |getRegions : Retrieves the Regions specified in the list of codes and from the provided tree.
getRegions:: [Code] -> SPtree -> Int -> [(String, Region)]
getRegions codes tree level = let
		filtered = L.filter (\(c,r)->L.elem c codes) (S.index tree 0)
	in filtered

-- |getRegionsUnder : Retrieves the all the regions under a specified level.
getRegionsUnder:: SPtree -> Int -> [(String, Region)]
getRegionsUnder tree level = 
	let 
		maps = toList subseq
		subseq = S.take level tree
	in concat maps

-- |maybeInsert : If an element is provided in maybe, it inserts it on the list. Else, nothing is done.
maybeInsert :: Maybe a -> [a] -> [a]
maybeInsert (Just x) list = x:list
maybeInsert Nothing list = list
