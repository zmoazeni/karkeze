module CLI (
  printGrams
  ,readGram
  ,loadIndex
  ,parseAndPrint
  ,example
) where

import Database.LevelDB
import Storage
import Data.Serialize (Serialize, encode, decode)
import Parser
import Data.Map (toList)
import Data.ByteString (ByteString)

parseAndPrint :: FilePath -> IO ()
parseAndPrint filePath = do
  rawJsons <- readFile filePath
  putStrLn . show $ parseInvertedIndex rawJsons

printGrams :: DB -> IO ()
printGrams db = withIterator db [] printKeys
  where
    printKeys iter = do
      iterFirst iter
      keys' <- keys db
      case toStrings keys' of
           [] -> putStrLn "No grams stored"
           xs -> putStrLn $ "grams: " ++ xs

    toStrings :: [ByteString] -> String
    toStrings = unwords . map (degrammify . decode)
    degrammify (Right (Gram string)) = string
    degrammify (Left errorMsg)       = error $ "trying to degrammify " ++ errorMsg

readGram :: DB -> Gram -> IO ()
readGram db gram = do
  value <- get db [] (encode gram)
  case value of
    Just x  -> let Right indexes = decode x :: Either String [Index]
              in print indexes
    Nothing -> let Gram rawGram = gram in putStrLn $ "gram: [" ++ rawGram ++ "] not found"

example :: DB -> IO ()
example db = do
  put db [] (encode "foo") (encode "bar")
  get db [] (encode "foo") >>= print
  delete db [] (encode "foo")
  get db [] (encode "foo") >>= print

loadIndex :: DB -> FilePath -> IO ()
loadIndex db filePath = do
  rawJsons <- readFile filePath
  saveGrams db (grams' rawJsons)
  where grams' = toList . parseInvertedIndex

saveGrams :: (Serialize a, Serialize b) => DB -> [(a, b)] -> IO ()
saveGrams db pairs = mapM_ put' pairs
  where
    put' (gram, indexes) = put db [] (encode gram) (encode indexes)


