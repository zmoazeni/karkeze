module ConcurrencyTest (
  badConcurrency
) where

import Data.Serialize hiding (put, get)
import Control.Concurrent
import Control.Monad
import System.Posix
import Database.LevelDB
import Data.ByteString (ByteString)

badConcurrency :: DB -> IO ()
badConcurrency db = do
  let numberOfThreads = 61
      emptyArray = [] :: [String]
  channels <- replicateM numberOfThreads newChan
  put db [] (e "key") (e emptyArray)
  mapM_ (forkIO . printThread) channels

  putStrLn "before"
  mapM_ readChan channels
  putStrLn "after"
  a <- getArray
  print a
  print $ length a
  where printThread channel = do
                                usleep 100
                                threadId <- myThreadId
                                let sThreadId = show threadId
                                putStrLn $ "in " ++ sThreadId
                                array <- getArray
                                put db [] (e "key") (e $ sThreadId:array)
                                writeChan channel "done"
        getArray :: IO [String]
        getArray = do
                      Just rawArr <- get db [] (e "key")
                      let Right array = d rawArr
                      return array

        e :: Serialize a => a -> ByteString
        e = encode

        d :: Serialize a => ByteString -> Either String a 
        d = decode
