{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Maps.Spatial (
	Region(..)) where

import Diagrams.Path 
import Diagrams.Prelude hiding (deep)

-- | SPATIAL MODULE --------------------------------------------------------------------------------------------------
-- ...................................................................................................................
-- ___________________________________________________________________________________________________________________

-- |The 'Region' It is a type definition that contains the class or identifier of the shape and a Diagram's path.
-- A path is a set of located trails.
data Region = Leaf { code :: String, coor :: [Path V2 Double]} | Branch {code :: String, nested :: [Region]}
  deriving (Show, Eq)

--groupRegions:: String -> [String] -> Region -> Region
--groupRegions ncode [] regions = regions
--groupRegions ncode [x:xs] Branch b = 

--groupRegions:: String -> [String] -> Region -> Region
--groupRegions ncode [] region = region
--groupRegions ncode list@[x:xs] region = 
-- 	| containsAnyRegion list region = region
-- 	| otherwise = region
--	| otherwise = foldl (\acc x -> if deepContainsRegion ncode x then True else acc) False (nested region)

nestRegion :: String -> Region -> Region

appendToRegion :: Region -> Region -> Region
appendToRegion leaf root@ Branch {nested = l} = root { nested = leaf : l }
appendToRegion _ region = region

deleteRegion :: String -> Region -> Region
deleteRegion c root@ Branch {nested = l} = root { nested = filter ( \l -> code l /= c) l }
deleteRegion _ region = region

containsAnyRegion:: [String] -> Region -> Bool
containsAnyRegion ncodes region = any (deepContainsRegion region) ncodes

deepContainsRegion :: Region -> String -> Bool
deepContainsRegion region ncode
	| containsRegion region ncode = True
	| otherwise = foldl (\acc x -> if deepContainsRegion x ncode then True else acc) False (nested region)

containsRegion:: Region -> String -> Bool
containsRegion Leaf {} ncode = False
containsRegion Branch {nested = b} ncode = any (isRegion ncode) b

isRegion :: String -> Region -> Bool
isRegion ncode Leaf {code = ccode} = ncode == ccode
isRegion ncode Branch {code = ccode} = ncode == ccode
isRegion ncode _ = False
