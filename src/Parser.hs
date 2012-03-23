{-# LANGUAGE DeriveDataTypeable #-}
module Parser (
  parseInvertedIndex
  , Gram (..)
  , Index (..)
  , decodeString
) where

import Data.Char
import Data.JSON2 (Json(..), ToJson, toJson)
import Data.JSON2.Parser (parseJson)
import Data.Map as M (Map, insertWith, empty, toList, unionWith, singleton)
import qualified Data.Map as M (lookup)
import Data.List (nub)
import Data.Serialize
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString)
import Data.Typeable

type JsonMap    = Map String Json
type IndexId    = Json
type Field      = String
type FieldText  = String
type RawJson    = String

data Gram = Gram String
  deriving (Eq, Ord, Show, Typeable)

data Index = Index IndexId Field
  deriving (Eq, Show)

instance Serialize Gram where
  put (Gram gramValue) = put . encodeUtf8 $ pack gramValue
  get = get >>= return . Gram . decodeString

instance ToJson Gram where
  toJson (Gram gramValue) = toJson gramValue

instance Serialize Index where
  put (Index indexId field) = let bType = encode $ indexType indexId
                                  bIndexId = encodeJson indexId
                                  bField = encodeString field
                              in put (bType, bIndexId, bField)
                              where encodeJson (JNumber i) = encode i
                                    encodeJson (JString s) = encodeString s
                                    encodeJson _           = error "Unknown id type"
                                    encodeString = encodeUtf8 . pack

  get = do
           (bIndexType, bIndexId, bField) <- get
           let iType = decode bIndexType :: Either String Integer
               field = decodeString bField
               indexId = case iType of
                              Right 0 -> JNumber $ decodeNumber bIndexId
                              Right 1 -> JString $ decodeString bIndexId
                              Right x -> error $ "Unknown IndexId type [" ++ show x ++ "]"
                              Left errorMsg -> error errorMsg
           return (Index indexId field)
           where decodeNumber bIndexId = case decode bIndexId of
                                              Right indexId -> indexId :: Rational
                                              Left errorMessage -> error errorMessage
decodeString :: ByteString -> String
decodeString = unpack . decodeUtf8

indexType :: IndexId -> Integer
indexType (JNumber _) = 0
indexType (JString _) = 1
indexType j = error $ "Unkown type for " ++ show j ++ "]"

parseInvertedIndex :: String -> Map Gram [Index]
parseInvertedIndex = splitGrams . textAndIndex . stringToJson

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

stringToJson :: RawJson -> [JsonMap]
stringToJson = map processJson . lines
  where processJson rawJson = case parseJson rawJson of
                                   Right (JObject x)  -> x
                                   Right _            -> error "JSON is incorrect format"
                                   Left _             -> empty

