module CLI (
  printGrams
  ,readGram
  ,loadIndex
  ,parseAndPrint
  ,example
) where

import Storage
import Parser
import Conversions
import Database.LevelDB
import Data.Map (toList)

parseAndPrint :: FilePath -> IO ()
parseAndPrint filePath = do
  rawJsons <- readFile filePath
  putStrLn . show $ parseInvertedIndex rawJsons

printGrams :: DB -> IO ()
printGrams db = withIterator db [] printKeys
  where printKeys iter = do
          iterFirst iter
          keys' <- keys db
          case toStrings keys' of
               [] -> putStrLn "No grams stored"
               xs -> putStrLn $ "grams: " ++ xs

        toStrings = unwords . map (degrammify . decode')
        degrammify (Gram string) = string

readGram :: DB -> Gram -> IO ()
readGram db gram = do
  value <- get db [] (encode' gram)
  case value of
    Just x  -> let indexes = decode' x :: [Index]
               in print indexes
    Nothing -> let Gram rawGram = gram in putStrLn $ "gram: [" ++ rawGram ++ "] not found"

example :: DB -> IO ()
example db = do
  put db [] (encode' "foo") (encode' "bar")
  get db [] (encode' "foo") >>= print
  delete db [] (encode' "foo")
  get db [] (encode' "foo") >>= print

loadIndex :: DB -> FilePath -> IO ()
loadIndex db filePath = do
  rawJsons <- readFile filePath
  saveGrams db (toGrams rawJsons)
  where toGrams = toList . parseInvertedIndex
