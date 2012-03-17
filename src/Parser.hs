module Parser (
  parseInvertedIndex,
  toJson,
  Gram (..)
) where

import Data.Char
import Data.JSON2 (Json(..))
import Data.JSON2.Parser
import Data.JSON2.Query
import Data.Map as M (Map, insert, insertWith, empty, toList)
import qualified Data.Map as M (lookup, map)

type JsonObject = Map String Json
data Gram = Gram String
  deriving (Eq, Ord, Show)

parseInvertedIndex :: String -> Map Gram [Json]
parseInvertedIndex = invertIdsAndGrams . idsAndGrams . idsAndText . toJson

invertIdsAndGrams :: Map Json [Gram] -> Map Gram [Json]
invertIdsAndGrams = foldr mapGramsToIds empty . toList
  where 
    mapGramsToIds (id, []) gramIdMap = gramIdMap
    mapGramsToIds (id, (gram:grams)) gramIdMap = insertWith uniqInsert gram [id] $ mapGramsToIds (id, grams) gramIdMap
    uniqInsert (newValue:newValues) oldValues = if newValue `elem` oldValues then oldValues else newValue:oldValues

idsAndGrams :: Map Json String -> Map Json [Gram]
idsAndGrams = M.map toGrams
  where toGrams string = map grammify . words $ map toLower string
        grammify x = Gram x

idsAndText :: [JsonObject] -> Map Json String
idsAndText jsons = foldr mapIdToText empty jsons
  where mapIdToText json map       = insert (getId $ M.lookup "id" json) (getText $ M.lookup "text" json) map
        getId (Just x)             = x
        getText (Just (JString x)) = x

toJson :: String -> [JsonObject]
toJson = map processJson . lines
  where processJson rawJson = case parseJson rawJson of
                                Right (JObject x)  -> x
                                Left _             -> empty
