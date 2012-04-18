{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseIndex
  ,Gram (..)
  ,Index (..)
  ,ParsedIndex (..)
  ,parseIds
) where

import Data.List (nub)
import Data.Binary
import Control.Monad
import Data.Aeson as A
import Data.Attoparsec.Lazy
import Data.HashMap.Lazy as HM (HashMap, insertWith, empty, toList, unionWith, singleton, (!), keys)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import qualified Data.Text.Encoding as TE
import Data.Maybe
import Data.Hashable
import qualified Data.ByteString.Lazy as BL

data Gram = Gram T.Text
          deriving (Eq, Ord, Show)

data Index = Index { indexId :: Value, indexField :: T.Text }
           deriving (Eq, Show)

data ParsedIndex = ParsedIndex { invertedIndex :: HashMap Gram [Index], idIndex :: (Value, [Gram])}

instance Binary Gram where
  put (Gram gramValue) = put $ TE.encodeUtf8 gramValue
  get = get >>= return . Gram . TE.decodeUtf8

instance ToJSON Gram where
  toJSON (Gram gramValue) = toJSON gramValue

instance Hashable Gram where
  hash (Gram gramValue) = hash gramValue
  hashWithSalt salt (Gram gramValue) = hashWithSalt salt gramValue

instance Binary Index where
  put index = do put . A.encode . V.fromList $ [indexId index]
                 put . TE.encodeUtf8 $ indexField index

  get = do id' <- liftM (V.head . fromJust . A.decode) get
           field <- liftM TE.decodeUtf8 get
           return (Index id' field)

parseIndex :: TL.Text -> ParsedIndex
parseIndex text = ParsedIndex {invertedIndex=invertedIndex', idIndex=idIndex'}
  where invertedIndex' = (splitGrams . textAndIndex . textToJson) text
        idIndex' = (id', keys invertedIndex')
        id' = indexId . head . snd . head $ toList invertedIndex'

parseIds :: BL.ByteString -> Value
parseIds raw = fromJust . maybeResult $ parse json raw

splitGrams :: HashMap T.Text [Index] -> HashMap Gram [Index]
splitGrams = foldr gramsToIndicies empty . toList
  where gramsToIndicies (text, indicies) gramMap = foldr (insertGrams indicies) gramMap $ T.words text
        insertGrams indicies rawGram gramMap = insertWith (\x y -> nub $ x ++ y) (Gram rawGram) indicies gramMap

textAndIndex :: Object -> HashMap T.Text [Index]
textAndIndex object' = foldr combine' empty $ singletons
  where allExceptId = [x | x <- toList object', let (key, _) = x, key /= "id"]
        singletons = map (toSingleton (object' ! "id")) allExceptId
        toSingleton id' (key, (String value)) = singleton (T.toLower value) [(Index id' key)]
        toSingleton _ _ = empty
        combine' kvPair textMap = unionWith (++) textMap kvPair

textToJson :: TL.Text -> Object
textToJson raw = case (eitherResult . parse json . encodeUtf8) raw of
  Right (Object x)  -> x
  Left msg          -> error $ "error parsing: " ++ msg
  _                 -> error $ "unexpected json"

