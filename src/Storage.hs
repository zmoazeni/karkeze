module Storage (
  withDB
  ,keys
  ,grams
  ,saveAction
  ,IndexAction (..)
  ,flush
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

saveAction :: DB -> IndexAction -> BL.ByteString -> IO ()
saveAction db action contents = do uid <- genUID
                                   put db [] uid (encode (action, contents))

genUID :: IO ByteString
genUID = do time <- getPOSIXTime
            return . hash SHA256 . encode $ show time

saveGrams :: (Serialize a, Serialize b) => DB -> [(a, b)] -> IO ()
saveGrams db pairs = mapM_ put' pairs
  where
    put' (gram, indexes) = put db [] (encode gram) (encode indexes)

flush :: DB -> DB -> IO ()
flush stageDB gramDB = withIterator stageDB [] $ \iter -> iterFirst iter >> flush' iter
  where flush' iter =
          do valid <- iterValid iter
             case valid of
               True -> do key <- iterKey iter
                          value <- iterValue iter
                          case decode value of
                            Right (IndexCreate, rawJson) -> saveGram rawJson
                            Left message                 -> error message
                            _                            -> error "Unknown action"
                          delete stageDB [] key
                          iterNext iter
                          flush' iter
               False -> return ()

        saveGram rawJson = print rawJson >> print (parseInvertedIndex rawJson) >> saveGrams gramDB (toList $ parseInvertedIndex rawJson)
