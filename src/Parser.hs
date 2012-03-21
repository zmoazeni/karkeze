module Parser (
  parseInvertedIndex
  , toJson
  , Gram (..)
  , Index (..)
) where

import Data.Char
import Data.JSON2 (Json(..))
import Data.JSON2.Parser
import Data.Map as M (Map, insertWith, empty, toList, unionWith, singleton)
import qualified Data.Map as M (lookup)
import Data.List (nub)
import Data.Serialize
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

type JsonMap    = Map String Json
type IndexId    = Json
type Field      = String
type FieldText  = String
type RawJson    = String

data Gram = Gram String
  deriving (Eq, Ord, Show)

data Index = Index IndexId Field
  deriving (Eq, Show)

instance Serialize Gram where
  put (Gram gramValue) = put . encodeUtf8 $ pack gramValue
  get = get >>= return . Gram . unpack . decodeUtf8

instance Serialize Index where
  put (Index indexId field) = let (JNumber i) = indexId
                                  bIndexId = encode i
                                  bField = encodeUtf8 $ pack field
                              in put (bIndexId, bField)

  get = do
          (bIndexId, bField) <- get
          let field = unpack $ decodeUtf8 bField
          case decode bIndexId of
               Right indexId -> return (Index (JNumber indexId) field)
               Left errorMessage -> error errorMessage

parseInvertedIndex :: String -> Map Gram [Index]
parseInvertedIndex = splitGrams . textAndIndex . toJson

splitGrams :: Map FieldText [Index] -> Map Gram [Index]
splitGrams = foldr gramsToIndicies empty . toList
  where
    gramsToIndicies (text, indicies) gramMap = foldr (insertGrams indicies) gramMap $ words text
    insertGrams indicies rawGram gramMap = insertWith (\x y -> nub $ x ++ y) (Gram rawGram) indicies gramMap

textAndIndex :: [JsonMap] -> Map FieldText [Index]
textAndIndex jsons = foldr combiner empty . concat $ map singletons jsons
  where
        combiner kvPair textMap = unionWith (++) textMap kvPair
        singletons json' = map (toSingleton (getId json')) $ [x | x <- toList json', let (key, _) = x, key /= "id"] 
        toSingleton indexId (key, (JString value)) = singleton (lowercase value) [(Index indexId key)]
        toSingleton _ _ = empty
        lowercase = map toLower

        getId json' = case M.lookup "id" json' of
                           Just x  -> x
                           Nothing -> error "We don't have an id"

toJson :: RawJson -> [JsonMap]
toJson = map processJson . lines
  where processJson rawJson = case parseJson rawJson of
                                   Right (JObject x)  -> x
                                   Right _            -> error "JSON is incorrect format"
                                   Left _             -> empty

