module Storage (
  withDB
  ,keys
  ,grams
) where

import Parser
import Database.LevelDB
import Data.Serialize (decode)
import Data.Either
import Data.ByteString (ByteString)

withDB :: FilePath -> (DB -> IO a) -> IO a
withDB filePath f = withLevelDB filePath [CreateIfMissing, CacheSize 1024] f

grams :: DB -> IO [Gram]
grams db = withIterator db [] $ \iter -> do
    iterFirst iter
    byteKeys <- keys iter
    let returnKeys = rights $ map (decode :: ByteString -> Either String Gram) byteKeys
    return returnKeys

keys :: Iterator -> IO [ByteString]
keys iter = do
  iterFirst iter
  getKeys []
  where
    getKeys :: [ByteString] -> IO [ByteString]
    getKeys xs = do valid <- iterValid iter
                    case valid of
                      True -> do
                                key <- iterKey iter
                                iterNext iter
                                otherKeys <- getKeys xs
                                return (key:otherKeys)
                      False -> return xs
