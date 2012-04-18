module Storage (
  withDB
  ,keys
  ,grams
  ,queueAction
  ,IndexAction (..)
  ,Databases (..)
  ,flush
  ,flushOnce
  ,saveGrams
  ,search
) where

import Parser
import Conversions
import Database.LevelDB
import qualified Data.Binary as Bin
import Data.Binary (Binary, Get, decode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Text.Lazy hiding (map, empty, null, filter)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX
import Codec.Digest.SHA
import Data.HashMap.Lazy (toList)
import Control.Monad
import Control.Concurrent
import Data.List (nub)
import Data.Vector as V (empty, fromList)
import qualified Data.Vector as V (toList)
import qualified Data.Aeson as A

data IndexAction = IndexCreate | IndexDelete | IndexUpdate
                 deriving (Eq, Ord, Show)

data Databases = Databases {gramDB :: DB, stageDB :: DB, idDB :: DB}

instance Binary IndexAction where
  put IndexCreate = Bin.put (0 :: Int)
  put IndexDelete = Bin.put (1 :: Int)
  put _           = error "Unimplemented"

  get = do i <- Bin.get :: Get Int
           case i of
             0 -> return IndexCreate
             1 -> return IndexDelete
             e -> error $ "Unknown Index Action " ++ (show e)


withDB :: FilePath -> (DB -> IO a) -> IO a
withDB filePath f = withLevelDB filePath [CreateIfMissing, CacheSize 1024] f

grams :: Databases -> IO [Gram]
grams Databases {gramDB=db} = withIterator db [] $ \iter -> do
  iterFirst iter
  byteKeys <- keys db
  return $ map (decode . byteStringToLazy) byteKeys

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

deleteGrams :: Databases -> BL.ByteString -> IO ()
deleteGrams Databases{idDB=idDB', gramDB=gramDB'} rawIds = do let A.Array v = parseIds rawIds
                                                                  ids = V.toList v
                                                              mapM_ deleteId ids
  where deleteId id' = do v <- get idDB' [] (byteStringFromLazy $ A.encode id')
                          case v of
                            Just x -> do let grams' = decode' x :: [Gram]
                                         mapM_ (deleteIdByGram id') grams'
                                         delete idDB' [] (byteStringFromLazy $ A.encode id')
                            _      -> return ()
                            
        deleteIdByGram id' gram = do v <- get gramDB' [] (encode' gram)
                                     case v of
                                       Just x -> do let indexes = decode' x :: [Index]
                                                        filtered = filter (\i -> indexId i /= id') indexes
                                                    put gramDB' [] (encode' gram) (encode' filtered)
                                       _      -> return ()
                              
saveGrams :: DB -> [(Gram, [Index])] -> IO ()
saveGrams db pairs = mapM_ put' pairs
  where put' (gram, indexes) = do
          let key = encode' gram
          maybeExisting <- get db [] key
          let value = case maybeExisting of
                           Just binaryIndexes -> encode' . (indexes ++) $ (decode' binaryIndexes :: [Index])
                           Nothing -> encode' indexes
          put db [] key value

saveId :: DB -> (A.Value, [Gram]) -> IO ()
saveId db (id', grams') = put db [] (byteStringFromLazy $ A.encode id') (encode' grams')

queueAction :: DB -> IndexAction -> BL.ByteString -> IO ()
queueAction db action contents = do let value = encode' (action, contents)
                                    uid <- genUID
                                    put db [] uid value

genUID :: IO ByteString
genUID = do time <- getPOSIXTime
            return . hash SHA256 . encode $ show time

flushOnce :: Databases -> IO ()
flushOnce dbs@Databases {stageDB=stageDB', gramDB=gramDB'} = withIterator stageDB' [] flush'
  where flush' = flushIterator dbs

flush :: Databases -> IO ()
flush dbs = forever $ flushOnce dbs

flushIterator :: Databases -> Iterator -> IO ()
flushIterator dbs@Databases{stageDB=stageDB', gramDB=gramDB', idDB=idDB'} iter = iterFirst iter >> iterValid iter >>= flush'
  where save raw = do let parsed = parseIndex raw
                      saveGrams gramDB' . toList $ invertedIndex parsed
                      saveId idDB' $ idIndex parsed

        flush' valid
          | valid     = do key   <- iterKey iter
                           value <- iterValue iter
                           case decode' value :: (IndexAction, BL.ByteString) of
                             (IndexCreate, rawJson) -> save $ decodeUtf8 rawJson
                             (IndexDelete, rawJson) -> deleteGrams dbs rawJson
                             _                      -> error "Unknown action"
                           delete stageDB' [] key
                           iterNext iter
                           iterValid iter >>= flush'
          | otherwise = yield

search :: DB -> Text -> [Text] -> IO A.Value
search db query fields = do
  let gram = Gram (toStrict query)
  maybeValue <- get db [] (encode' gram)
  case maybeValue of
    Just binaryIndexes -> do let indexes = decode' binaryIndexes :: [Index]
                                 filtered = [x | x <- indexes, indexField x `elem` (map toStrict fields)]
                                 xs = if null fields then indexes else filtered
                             return . A.Array . V.fromList . nub $ map indexId xs
    Nothing            -> return $ A.Array (empty)

