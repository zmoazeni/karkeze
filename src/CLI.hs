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
import Data.HashMap.Lazy (toList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

parseAndPrint :: FilePath -> IO ()
parseAndPrint filePath = do
  rawJsons <- readFile filePath
  putStrLn . show . parseInvertedIndex $ TL.pack rawJsons

printGrams :: DB -> IO ()
printGrams db = withIterator db [] printKeys
  where printKeys iter = do
          iterFirst iter
          keys' <- keys db
          let textGrams = toText keys'
          case T.null textGrams of
               True  -> putStrLn "No grams stored"
               False -> putStrLn $ "grams: " ++ (T.unpack textGrams)

        toText = T.unwords . map (degrammify . decode')
        degrammify (Gram text) = text

readGram :: DB -> Gram -> IO ()
readGram db gram = do
  value <- get db [] (encode' gram)
  case value of
    Just x  -> let indexes = decode' x :: [Index]
               in print indexes
    Nothing -> let Gram rawGram = gram in putStrLn $ "gram: [" ++ (T.unpack rawGram) ++ "] not found"

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
  where toGrams = toList . parseInvertedIndex . TL.pack
