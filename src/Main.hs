{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Parser
import Storage
import Web
import CLI
import Control.Concurrent
import qualified Control.Concurrent.Thread.Group as ThreadGroup
--import Control.Concurrent.MVar
import Data.Text
import System.Posix.Signals

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
      ("web":port:_)         -> startWeb dbs port
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
  
startWeb :: Databases -> String -> IO ()
startWeb dbs port = do threads <- ThreadGroup.new
                       keepRunning <- newMVar True
                       _ <- ThreadGroup.forkIO threads (flush dbs keepRunning)
                       (tId, _) <- ThreadGroup.forkIO threads $ run port dbs
                       _ <- installHandler sigINT (Catch (handler keepRunning tId)) Nothing
                       ThreadGroup.wait threads
                       
  where handler keepRunning webThreadId = do putStrLn "Shutting down..."
                                             _ <- swapMVar keepRunning False
                                             killThread webThreadId
                                             return ()