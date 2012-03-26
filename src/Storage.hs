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
  byteKeys <- keys db
  let returnKeys = rights $ map (decode :: ByteString -> Either String Gram) byteKeys
  return returnKeys

keys :: DB -> IO [ByteString]
keys db = withIterator db [] doGetKeys
  where doGetKeys iter = iterFirst iter >> getKeys [] iter

        getKeys :: [ByteString] -> Iterator -> IO [ByteString]
        getKeys xs iter = do valid <- iterValid iter
                             case valid of
                                  True -> do
                                            key <- iterKey iter
                                            iterNext iter
                                            otherKeys <- getKeys xs iter
                                            return (key:otherKeys)
                                  False -> return xs
