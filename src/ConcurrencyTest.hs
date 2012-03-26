module ConcurrencyTest (
  badConcurrency
  ,separateKeys
) where

import Data.Serialize hiding (put, get)
import Control.Concurrent
import Control.Monad
import System.Posix
import Database.LevelDB
import Data.ByteString (ByteString)
import Storage

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

separateKeys :: DB -> IO ()
separateKeys db = do
  let numberOfThreads = 521
  channels <- replicateM numberOfThreads newChan
  mapM_ (forkIO . printThread) channels

  putStrLn "before"
  mapM_ readChan channels
  putStrLn "after"
  keys db >>= print . length
  where printThread channel = do
                                usleep 100
                                threadId <- myThreadId
                                let sThreadId = show threadId
                                put db [] (e sThreadId) (e (1::Int))
                                writeChan channel "done"

e :: Serialize a => a -> ByteString
e = encode

d :: Serialize a => ByteString -> Either String a 
d = decode
