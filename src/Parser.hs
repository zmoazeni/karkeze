module Parser (
  parseInvertedIndex,
  toJson,
  Gram (..)
) where

import Data.Char
import Data.JSON2 (Json(..))
import Data.JSON2.Parser
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
    mapGramsToIds (_, []) gramIdMap = gramIdMap
    mapGramsToIds (jsonId, (gram:grams)) gramIdMap = insertWith uniqInsert gram [jsonId] $ mapGramsToIds (jsonId, grams) gramIdMap
    uniqInsert (newValue:_) oldValues = if newValue `elem` oldValues then oldValues else newValue:oldValues
    uniqInsert [] _ = []

idsAndGrams :: Map Json String -> Map Json [Gram]
idsAndGrams = M.map toGrams
  where toGrams string = map Gram . words $ map toLower string

idsAndText :: [JsonObject] -> Map Json String
idsAndText jsons = foldr mapIdToText empty jsons
  where mapIdToText json resultMap       = insert (getId $ M.lookup "id" json) (getText $ M.lookup "text" json) resultMap
        getId (Just x)             = x
        getId Nothing              = error "We don't have an id"
        getText (Just (JString x)) = x
        getText _                  = error "We don't have text"

toJson :: String -> [JsonObject]
toJson = map processJson . lines
  where processJson rawJson = case parseJson rawJson of
                                Right (JObject x)  -> x
                                Right _            -> error "JSON is incorrect format"
                                Left _             -> empty
