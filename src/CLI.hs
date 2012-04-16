module CLI (
  printGrams
  ,readGram
  ,loadIndex
  ,parseAndPrint
  ,example
  ,gramKeys
) where

import Storage
import Parser
import Conversions
import Database.LevelDB
import Data.HashMap.Lazy (toList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.ByteString (ByteString)

parseAndPrint :: FilePath -> IO ()
parseAndPrint filePath = do
  rawJsons <- readFile filePath
  putStrLn . show . parseInvertedIndex $ TL.pack rawJsons
  
gramKeys :: Databases -> IO [ByteString]
gramKeys Databases {gramDB=db} = keys db

printGrams :: Databases -> IO ()
printGrams Databases {gramDB=db} = withIterator db [] printKeys
  where printKeys iter = do
          iterFirst iter
          keys' <- keys db
          let textGrams = toText keys'
          case T.null textGrams of
               True  -> putStrLn "No grams stored"
               False -> putStrLn $ "grams: " ++ (T.unpack textGrams)

        toText = T.unwords . map (degrammify . decode')
        degrammify (Gram text) = text

readGram :: Databases -> Gram -> IO ()
readGram Databases {gramDB=db} gram = do
  value <- get db [] (encode' gram)
  case value of
    Just x  -> let indexes = decode' x :: [Index]
               in print indexes
    Nothing -> let Gram rawGram = gram in putStrLn $ "gram: [" ++ (T.unpack rawGram) ++ "] not found"

example :: Databases -> IO ()
example Databases {gramDB=db} = do
  put db [] (encode' "foo") (encode' "bar")
  get db [] (encode' "foo") >>= print
  delete db [] (encode' "foo")
  get db [] (encode' "foo") >>= print

loadIndex :: Databases -> FilePath -> IO ()
loadIndex Databases {gramDB=db} filePath = do
  rawJsons <- readFile filePath
  saveGrams db (toGrams rawJsons)
  where toGrams = toList . parseInvertedIndex . TL.pack
