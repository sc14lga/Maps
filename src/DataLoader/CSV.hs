{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module DataLoader.CSV ( 
	parseCSV,
	parseCSVstruc
	) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import Maps.DataType
import qualified Data.Csv as C

-- |The polymorphic data class CSVParser enables the direct parsing of CSVs into datasets
class CSVParser a where
	-- | decode : Decodes a CSV into a Vector of tuples with a String (code) and the specified polymorphic type
	decode :: BL.ByteString -> Either String (V.Vector (String, a))
	-- | decoderel : Decodes a CSV into a Vector of triples with two String (codes) and the specified polymorphic type
	decoderel :: BL.ByteString -> Either String (V.Vector (String, String, a))
	-- | parseCSV : Can parse a contextable type generating a dataset of this type. The second parameter, integer, defines
	-- the length of the set of codes. However, only length one and two are implemented (entities and simple relations). 
	-- Therefore, if the integer is other than 1 or 2, an EntitySet will be generated as it is the default one.
	parseCSV :: (Contextable a) => BL.ByteString -> Int -> DataSet a
	parseCSV file 2 = let 
			v = decoderel file
			l = V.toList $ applyData v V.empty
		in generateRelationset l
	parseCSV file i = let
			v = decode file
			l = V.toList $ applyData v V.empty
		in generateDataset l
	
-- | Instance of CSVParser for StvNominal : implementation of decoderel and decode
instance CSVParser StvNominal where
	decoderel file = C.decode C.NoHeader file::Either String (V.Vector (String, String, StvNominal))
	decode file = C.decode C.NoHeader file::Either String (V.Vector (String, StvNominal))

-- | Instance of CSVParser for StvOrdinal : implementation of decoderel and decode
instance CSVParser StvOrdinal where
	decoderel file = C.decode C.NoHeader file::Either String (V.Vector (String, String, StvOrdinal))
	decode file = C.decode C.NoHeader file::Either String (V.Vector (String, StvOrdinal))

-- | Instance of CSVParser for StvRatio : implementation of decoderel and decode
instance CSVParser StvRatio where
	decoderel file = C.decode C.NoHeader file::Either String (V.Vector (String, String, StvRatio))
	decode file = C.decode C.NoHeader file::Either String (V.Vector (String, StvRatio))

-- | parseCSVstruc : Function to load a structure of regions, as it is provided by the World Bank
parseCSVstruc :: BL.ByteString -> [(String, [String])]
parseCSVstruc file = let
			v = C.decode C.NoHeader file::Either String (V.Vector (String, String))
			l = V.toList $ applyData v V.empty
			grouped = parseMap l
			parseMap list = foldl (\map (code, son) -> M.insertWith (mixRecord) code [son] map) (M.empty) list
			mixRecord original new = original L.++ new
		in M.toList grouped

-- | applyData : Utility function to translate an Either a b to the value placed in the Left position.
--   it does return that value if Left come and a default one otherwise, provided as second argument.
applyData :: Either b a -> a -> a
applyData (Right x) _ = x 
applyData _ v = v
