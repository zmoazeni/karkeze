module Storage (
  withDB
  ,keys
  ,grams
  ,databasePath
) where

import Parser
import Database.LevelDB
import Data.Serialize (decode)
import Data.Either
import Data.ByteString (ByteString)

databasePath :: FilePath
databasePath = "./db/leveldbtest"

withDB :: (DB -> IO a) -> IO a
withDB f = withLevelDB databasePath [CreateIfMissing, CacheSize 1024] f

grams :: DB -> IO [Gram]
grams db = withIterator db [] $ \iter -> do
    iterFirst iter
    byteKeys <- keys iter []
    let returnKeys = rights $ map (decode :: ByteString -> Either String Gram) byteKeys
    return returnKeys

keys :: Iterator -> [ByteString] -> IO [ByteString]
keys iter returnKeys = do
  valid <- iterValid iter
  case valid of
      True -> do
        key <- iterKey iter
        iterNext iter
        otherKeys <- keys iter returnKeys
        return (key:otherKeys)
      False -> return (returnKeys)

