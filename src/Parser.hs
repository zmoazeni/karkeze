{-# LANGUAGE DeriveDataTypeable #-}
module Parser (
  parseInvertedIndex
  , Gram (..)
  , Index (..)
) where

import Data.Char
import Data.JSON2 (Json(..), ToJson, toJson)
import Data.JSON2.Parser (parseJson)
import Data.Map as M (Map, insertWith, empty, toList, unionWith, singleton)
import qualified Data.Map as M (lookup)
import Data.List (nub)
import Data.Binary
import Data.Typeable
import Control.Monad
import Conversions

type JsonMap    = Map String Json
type IndexId    = Json
type Field      = String
type FieldText  = String
type RawJson    = String

data Gram = Gram String
  deriving (Eq, Ord, Show, Typeable)

data Index = Index { indexId :: IndexId, indexField :: Field }
  deriving (Eq, Show)

instance Binary Gram where
  put (Gram gramValue) = put $ stringToByteString gramValue
  get = get >>= return . Gram . byteStringToString

instance ToJson Gram where
  toJson (Gram gramValue) = toJson gramValue

instance Binary Index where
  put index = do let field = indexField index
                     id' = indexId index
                 put $ indexType id'
                 case indexId index of
                      JNumber i -> put i
                      JString s -> put $ stringToByteString s
                      _         -> error "Unknown id type"
                 put $ stringToByteString field
              where indexType :: Json -> Int
                    indexType (JNumber _) = 0
                    indexType (JString _) = 1
                    indexType j = error $ "Unkown type for " ++ show j ++ "]"

  get = do indexType <- get :: Get Int
           id' <- case indexType of
                       0 -> liftM JNumber get
                       1 -> liftM (JString . byteStringToString) get
                       _ -> error "Unknown type"
           field <- liftM byteStringToString get
           return (Index id' field)

parseInvertedIndex :: String -> Map Gram [Index]
parseInvertedIndex = splitGrams . textAndIndex . stringToJson

splitGrams :: Map FieldText [Index] -> Map Gram [Index]
splitGrams = foldr gramsToIndicies empty . toList
  where gramsToIndicies (text, indicies) gramMap = foldr (insertGrams indicies) gramMap $ words text
        insertGrams indicies rawGram gramMap = insertWith (\x y -> nub $ x ++ y) (Gram rawGram) indicies gramMap

textAndIndex :: [JsonMap] -> Map FieldText [Index]
textAndIndex jsons = foldr combiner empty . concat $ map singletons jsons
  where combiner kvPair textMap = unionWith (++) textMap kvPair
        singletons json' = map (toSingleton (getId json')) $ [x | x <- toList json', let (key, _) = x, key /= "id"] 
        toSingleton id' (key, (JString value)) = singleton (lowercase value) [(Index id' key)]
        toSingleton _ _ = empty
        lowercase = map toLower

        getId json' = case M.lookup "id" json' of
                           Just x  -> x
                           Nothing -> error "We don't have an id"

stringToJson :: RawJson -> [JsonMap]
stringToJson = map processJson . lines
  where processJson rawJson = case parseJson rawJson of
                                   Right (JObject x)  -> x
                                   Right _            -> error "JSON is incorrect format"
                                   Left _             -> empty

