{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseInvertedIndex
  ,Gram (..)
  ,Index (..)
) where

import Data.List (nub)
import Data.Binary
import Control.Monad
import Data.Aeson as A
import Data.Attoparsec.Lazy
import Data.HashMap.Lazy as HM (HashMap, insertWith, empty, toList, unionWith, singleton)
import qualified Data.HashMap.Lazy as HM (lookup)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import qualified Data.Text.Encoding as TE
import Data.Maybe
import Data.Hashable

data Gram = Gram T.Text
          deriving (Eq, Ord, Show)

data Index = Index { indexId :: Value, indexField :: T.Text }
           deriving (Eq, Show)

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

parseInvertedIndex :: TL.Text -> HashMap Gram [Index]
parseInvertedIndex = splitGrams . textAndIndex . stringToJson

splitGrams :: HashMap T.Text [Index] -> HashMap Gram [Index]
splitGrams = foldr gramsToIndicies empty . toList
  where gramsToIndicies (text, indicies) gramMap = foldr (insertGrams indicies) gramMap $ T.words text
        insertGrams indicies rawGram gramMap = insertWith (\x y -> nub $ x ++ y) (Gram rawGram) indicies gramMap

textAndIndex :: [Object] -> HashMap T.Text [Index]
textAndIndex = foldr combiner empty . concat . map singletons
  where singletons object' = map (toSingleton (getId object')) $ [x | x <- toList object', let (key, _) = x, key /= "id"]
        toSingleton id' (key, (String value)) = singleton (T.toLower value) [(Index id' key)]
        toSingleton _ _ = empty
        combiner kvPair textMap = unionWith (++) textMap kvPair

        getId object' = case HM.lookup "id" object' of
                             Just x  -> x
                             Nothing -> error "We don't have an id"

stringToJson :: TL.Text -> [Object]
stringToJson = map processJson . TL.lines
  where processJson rawJsonText = let rawJson = encodeUtf8 rawJsonText
                                  in case eitherResult $ parse json rawJson of
                                          Right (Object x)  -> x
                                          Left msg          -> error $ "error parsing: " ++ msg
                                          _                 -> error $ "unexpected json"

