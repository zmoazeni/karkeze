module Storage (
  withDB
  ,keys
  ,grams
  ,queueAction
  ,IndexAction (..)
  ,flush
  ,flushOnce
  ,saveGrams
) where

import Parser
import Database.LevelDB
import Data.Serialize (Serialize, encode, decode)
import qualified Data.Serialize as S (put, get)
import Data.Either
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Time.Clock.POSIX
import Codec.Digest.SHA
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (pack, unpack)
import Data.Map (toList)
import Control.Monad
import Control.Concurrent

data IndexAction = IndexCreate | IndexDelete
  deriving (Eq, Ord, Show)

instance Serialize IndexAction where
  put IndexCreate = S.put . encodeUtf8 $ pack "create"
  put _           = error "Unimplemented"

  get = do string <- S.get
           case unpack $ decodeUtf8 string of
             "create" -> return IndexCreate
             e        -> error $ "Unknown Index Action " ++ e

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

saveGrams :: (Serialize a, Serialize b) => DB -> [(a, b)] -> IO ()
saveGrams db pairs = mapM_ put' pairs
  where
    put' (gram, indexes) = put db [] (encode gram) (encode indexes)

queueAction :: DB -> IndexAction -> BL.ByteString -> IO ()
queueAction db action contents = do uid <- genUID
                                    put db [] uid (encode (action, contents))

genUID :: IO ByteString
genUID = do time <- getPOSIXTime
            return . hash SHA256 . encode $ show time

flushOnce :: DB -> DB -> IO ()
flushOnce stageDB gramDB = withIterator stageDB [] flush'
  where flush' = flushIterator stageDB gramDB

flush :: DB -> DB -> IO ()
flush stageDB gramDB = forever $ flushOnce stageDB gramDB

flushIterator :: DB -> DB -> Iterator -> IO ()
flushIterator stageDB gramDB iter = iterFirst iter >> iterValid iter >>= flush'
  where save = saveGrams gramDB . toList . parseInvertedIndex
        flush' valid
          | valid     = do key   <- iterKey iter
                           value <- iterValue iter
                           case decode value of
                             Right (IndexCreate, rawJson) -> save rawJson
                             Left message                 -> error message
                             _                            -> error "Unknown action"
                           delete stageDB [] key
                           iterNext iter
                           iterValid iter >>= flush'
          | otherwise = yield

