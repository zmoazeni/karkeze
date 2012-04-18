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
  withDatabases $ \dbs ->
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

withDatabases :: (Databases -> IO()) -> IO()
withDatabases f = withDB gramDBPath $ \gramDB' ->
  withDB stageDBPath $ \stageDB' ->
  withDB idDBPath $ \idDB' ->
  f $ Databases {gramDB=gramDB', stageDB=stageDB', idDB=idDB'}
     
  where gramDBPath  = "./db/db-grams"
        stageDBPath = "./db/db-stage"
        idDBPath    = "./db/db-ids"
  
