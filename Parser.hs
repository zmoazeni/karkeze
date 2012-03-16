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

parseInvertedIndex :: String -> M.Map String [Integer]
parseInvertedIndex = invertIdsAndGrams . idsAndGrams . idsAndText . toJson

invertIdsAndGrams :: M.Map Integer [String] -> M.Map String [Integer]
invertIdsAndGrams idGramMap = foldr mapGramsToIds M.empty $ M.toList idGramMap
  where 
    mapGramsToIds (id, []) gramIdMap = gramIdMap
    mapGramsToIds (id, (gram:grams)) gramIdMap = M.insertWith uniqInsert gram [id] $ mapGramsToIds (id, grams) gramIdMap
    uniqInsert (newValue:newValues) oldValues = if newValue `elem` oldValues then oldValues else newValue:oldValues

idsAndGrams :: M.Map Integer String -> M.Map Integer [String]
idsAndGrams idGramMap = M.map toGrams idGramMap
  where toGrams string = words $ map toLower string

idsAndText :: [M.Map String Json] -> M.Map Integer String
idsAndText jsons = foldr mapIdToText M.empty jsons
  where mapIdToText json map       = M.insert (getId $ M.lookup "id" json) (getText $ M.lookup "text" json) map
        getId (Just (JNumber x))   = truncate x
        getText (Just (JString x)) = x

toJson :: String -> [M.Map String Json]
toJson rawJsons = (map processJson . lines) rawJsons
  where processJson rawJson = case parseJson rawJson of
                                Right (JObject x)  -> x
                                Left _             -> M.empty