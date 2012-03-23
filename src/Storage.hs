module Storage (
  printGrams
  ,readGram
  ,withDB
  ,loadIndex
  ,saveGrams
  ,grams
) where

import Parser
import Data.Map (toList)
import Database.LevelDB
import Data.Serialize (Serialize, encode, decode)
import Data.Either
import Data.ByteString (ByteString)

databasePath :: FilePath
databasePath = "./db/leveldbtest"

withDB :: (DB -> IO a) -> IO a
withDB f = withLevelDB databasePath [CreateIfMissing, CacheSize 1024] f

grams :: IO [Gram]
grams = withDB $ \db ->
  withIterator db [] $ \iter -> do
    iterFirst iter
    byteKeys <- getKeys iter []
    let keys = rights $ map (decode :: ByteString -> Either String Gram) byteKeys
    return keys

getKeys :: Iterator -> [ByteString] -> IO [ByteString]
getKeys iter keys = do
  valid <- iterValid iter
  case valid of
      True -> do
        key <- iterKey iter
        iterNext iter
        otherKeys <- getKeys iter keys
        return (key:otherKeys)
      False -> return (keys)

printGrams :: IO ()
printGrams = withDB $ \db ->
  withIterator db [] $ \iter -> do
    iterFirst iter
    keys <- getKeys iter []
    case toStrings keys of
         [] -> putStrLn "No grams stored"
         xs -> putStrLn $ "grams: " ++ xs
  where 
    toStrings keys = unwords $ map (degrammify . decode) keys
    degrammify (Right (Gram string)) = string
    degrammify (Left errorMsg)       = error $ "trying to degrammify " ++ errorMsg

readGram :: Gram -> IO ()
readGram gram = withDB $ \db -> do
                                   value <- get db [] (encode gram)
                                   case value of
                                     Just x  -> let Right indexes = decode x :: Either String [Index]
                                                in print indexes
                                     Nothing -> let Gram rawGram = gram in putStrLn $ "gram: [" ++ rawGram ++ "] not found"

loadIndex :: IO ()
loadIndex = do
               rawJsons <- readFile "input.json"
               destroy databasePath []
               withDB $ \db -> saveGrams db . toList $ parseInvertedIndex rawJsons
               putStrLn "input.json loaded"

saveGrams :: (Serialize a, Serialize b) => DB -> [(a, b)] -> IO ()
saveGrams db pairs = mapM_ put' pairs
  where
    put' (gram, indexes) = put db [] (encode gram) (encode indexes)

