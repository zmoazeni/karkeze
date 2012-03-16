module Parser (
  parseInvertedIndex,
  toJson
) where

import Data.List
import Data.Char
import Data.JSON2 (Json(..))
import Data.JSON2.Parser
import Data.JSON2.Query
import qualified Data.Map as M
import Data.Maybe

type Gram       = String
type JsonId     = Integer
type JsonObject = M.Map String Json

parseInvertedIndex :: String -> M.Map Gram [JsonId]
parseInvertedIndex = invertIdsAndGrams . idsAndGrams . idsAndText . toJson

invertIdsAndGrams :: M.Map JsonId [Gram] -> M.Map Gram [JsonId]
invertIdsAndGrams = foldr mapGramsToIds M.empty . M.toList
  where 
    mapGramsToIds (id, []) gramIdMap = gramIdMap
    mapGramsToIds (id, (gram:grams)) gramIdMap = M.insertWith uniqInsert gram [id] $ mapGramsToIds (id, grams) gramIdMap
    uniqInsert (newValue:newValues) oldValues = if newValue `elem` oldValues then oldValues else newValue:oldValues

idsAndGrams :: M.Map JsonId String -> M.Map JsonId [Gram]
idsAndGrams = M.map toGrams
  where toGrams string = words $ map toLower string

idsAndText :: [JsonObject] -> M.Map JsonId String
idsAndText jsons = foldr mapIdToText M.empty jsons
  where mapIdToText json map       = M.insert (getId $ M.lookup "id" json) (getText $ M.lookup "text" json) map
        getId (Just (JNumber x))   = truncate x
        getText (Just (JString x)) = x

toJson :: String -> [JsonObject]
toJson = map processJson . lines
  where processJson rawJson = case parseJson rawJson of
                                Right (JObject x)  -> x
                                Left _             -> M.empty