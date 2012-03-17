module Storage (
  printGrams
  ,readGram
  ,loadIndex
) where

import Parser
import Data.JSON2 (Json(..))
import Data.Map (toList)
import Database.LevelDB
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.Serialize as S (encode, decode)

data Encodable = EString String | EGram Gram | EJson Json | EJsons [Json]
data Decodable = ToDString ByteString | ToDGram ByteString | ToDJson ByteString | ToDJsons ByteString
data Decoded   = DString String | DGram Gram | DJson Json | DJsons [Json] deriving (Show)

databasePath :: FilePath
databasePath = "./db/leveldbtest"

withDB :: (DB -> IO a) -> IO a
withDB f = withLevelDB databasePath [CreateIfMissing, CacheSize 1024] f

printGrams :: IO ()
printGrams = do
  withDB $ \db -> do
    withIterator db [] $ \iter -> do
      iterFirst iter
      keys <- getKeys iter []
      case toStrings keys of
        [] -> putStrLn "No grams stored"
        xs -> putStrLn $ "grams: " ++ xs
  where 
    toStrings keys = unwords . map degrammify $ map (decode . ToDGram) keys
    getKeys iter keys = do
                          valid <- iterValid iter
                          case valid of
                            True -> do
                                      key <- iterKey iter
                                      iterNext iter
                                      otherKeys <- getKeys iter keys
                                      return (key:otherKeys)
                            False -> return (keys)
    degrammify (DGram (Gram string)) = string
    degrammify _                    = error "Unknown gram"

readGram :: Gram -> IO ()
readGram gram = do
  withDB $ \db -> do
    value <- get db [] (encode $ EGram gram)
    case value of
      Just x  -> let DJsons jsons = decode (ToDJsons x)
                     xs = map truncateJson jsons
                 in print xs
      Nothing -> let Gram rawGram = gram in putStrLn $ "gram: [" ++ rawGram ++ "] not found"

loadIndex :: IO ()
loadIndex = do
  rawJsons <- readFile "input.json"
  destroy databasePath []
  withDB $ \db -> saveGrams db . toList $ parseInvertedIndex rawJsons
  putStrLn "input.json loaded"

  where
    saveGrams db ((gram, jsons):xs) = put db [] (encode $ EGram gram) (encode $ EJsons jsons) >> saveGrams db xs
    saveGrams _ [] = return ()

encode :: Encodable -> ByteString
encode (EString string)      = encodeUtf8 $ pack string
encode (EGram (Gram string)) = encode $ EString string
encode (EJson (JNumber i))   = S.encode i
encode (EJson _)             = error "unsupported JSON id"
encode (EJsons jsons)        = S.encode $ map toNumber jsons


decode :: Decodable -> Decoded
decode (ToDString byteString) = DString . unpack $ decodeUtf8 byteString
decode (ToDGram byteString)   = let DString string = decode $ ToDString byteString in DGram (Gram string)
decode (ToDJson byteString)   = let Right x = S.decode byteString in DJson (JNumber x)
decode (ToDJsons byteString)  = let Right xs = S.decode byteString in DJsons $ map (JNumber) xs

toNumber :: Json -> Rational
toNumber (JNumber i) = i
toNumber _           = error "Unknown JSON id"

truncateJson :: Json -> Integer
truncateJson = truncate . toNumber
