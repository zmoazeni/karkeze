{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Parser
import Storage
import Web
import CLI
import Control.Concurrent
import Data.Text

main :: IO ()
main = do
  args <- getArgs
  withDatabases args $ \dbs ->
    case args of
      ("read":rawGram:_)     -> readGram dbs (Gram (pack rawGram))
      ("load":_)             -> loadIndex dbs "input.json"
      ("grams":_)            -> printGrams dbs
      ("rawGrams":_)         -> grams dbs >>= print
      ("rawKeys":_)          -> gramKeys dbs >>= print
      ("print":_)            -> parseAndPrint "input.json"
      ("web":port:_)         -> do _ <- forkIO $ flush dbs
                                   run port dbs
      ("flush":_)            -> flushOnce dbs
      ("example":_)          -> example dbs
      _                      -> putStrLn "[load|print|grams|read <gram>|web <port>|example|concurrencytest1|concurrencytest2]"

databasePath :: [String] -> FilePath
databasePath ("concurrencytest1":_) = "./db/db-concurrencytest"
databasePath ("concurrencytest2":_) = "./db/db-concurrencytest"
databasePath _                     = "./db/db-leveldbtest"

stageDBPath :: FilePath
stageDBPath = "./db/db-stage"

withDatabases :: [String] -> (Databases -> IO()) -> IO()
withDatabases args f = withDB (databasePath args) $ \db ->
  withDB stageDBPath $ \stageDB' ->
  let dbs = Databases {gramDB=db, stageDB=stageDB', idDB=db}
  in f dbs
  
