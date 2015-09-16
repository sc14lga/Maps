{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module MapLoader.SVG (parseFile) where

import Maps.Spatial
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Data.List.Split
import Diagrams.Path 
import Diagrams.Prelude hiding (deep)
import Diagrams.Backend.SVG.CmdLine

-- | SVG MODULE ------------------------------------------------------------------------------------------------------
-- ...................................................................................................................
-- ___________________________________________________________________________________________________________________

-- |The 'parseFile' takes a filepath and returns an array of Regions, generated with generateRegions.
parseFile :: FilePath -> IO [Region]
parseFile file = runX (readDocument [withValidate no] file >>> generateRegions)

-- |The 'generateRegions' takes the paths from the XML and extracts the contents to the save them on an array of Region. We use 
-- the function parsePath to actually process the string of coordinates that will define the Path
-- this function corresponds to a single p tag in the XML, which can contain several closed paths or subpaths
generateRegions = deep (isElem >>> hasName "path") >>> 
  proc x -> do
    cl <- getAttrValue "class" -< x
    co <- getAttrValue "d" -< x
    returnA -< Base { code = (last . words) cl, coor = parsePath co }

-- |The 'parsePath' function just splits the string by the zZ to be able to treat every closed Path by separated
--  we call parseSubPath for each one of them
parsePath :: String -> [Path V2 Double]
parsePath str = [ parseSubPath s | s <- splitOneOf "zZ" str]

-- |The 'parseSubPath' function transforms a subpath on a Diagram's Loop. It maps the coordinates inside 
parseSubPath :: String -> Path V2 Double
parseSubPath str = pathFromLocTrail . mapLoc closeTrail . fromVertices $ ( map p2 $ parseCoords str )


-- |The 'parseCoords' function transforms a coordinates string into a pair of doubles to be able to treat it 
parseCoords :: String -> [(Double, Double)] 
parseCoords str = [ read ("(" ++ s ++ ")") | s <- words str , length s > 1]
