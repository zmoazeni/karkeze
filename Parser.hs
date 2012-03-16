module Parser (
  parseInvertedIndex,
  toJson,
  Gram (..)
) where

import Data.List
import Data.Char
import Data.JSON2 (Json(..))
import Data.JSON2.Parser
import Data.JSON2.Query
import qualified Data.Map as M
import Data.Maybe

type JsonObject = M.Map String Json
data Gram = Gram String
  deriving (Eq, Ord, Show)

parseInvertedIndex :: String -> M.Map Gram [Json]
parseInvertedIndex = invertIdsAndGrams . idsAndGrams . idsAndText . toJson

invertIdsAndGrams :: M.Map Json [Gram] -> M.Map Gram [Json]
invertIdsAndGrams = foldr mapGramsToIds M.empty . M.toList
  where 
    mapGramsToIds (id, []) gramIdMap = gramIdMap
    mapGramsToIds (id, (gram:grams)) gramIdMap = M.insertWith uniqInsert gram [id] $ mapGramsToIds (id, grams) gramIdMap
    uniqInsert (newValue:newValues) oldValues = if newValue `elem` oldValues then oldValues else newValue:oldValues

idsAndGrams :: M.Map Json String -> M.Map Json [Gram]
idsAndGrams = M.map toGrams
  where toGrams string = map grammify . words $ map toLower string
        grammify x = Gram x

idsAndText :: [JsonObject] -> M.Map Json String
idsAndText jsons = foldr mapIdToText M.empty jsons
  where mapIdToText json map       = M.insert (getId $ M.lookup "id" json) (getText $ M.lookup "text" json) map
        getId (Just x)             = x
        getText (Just (JString x)) = x

toJson :: String -> [JsonObject]
toJson = map processJson . lines
  where processJson rawJson = case parseJson rawJson of
                                Right (JObject x)  -> x
                                Left _             -> M.empty