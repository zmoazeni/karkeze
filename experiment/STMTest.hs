import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Data.IORef
import System.Random
import System.Posix
import Control.Concurrent.Thread.Group as G
import GHC.Conc.Sync (unsafeIOToSTM)
import Control.Concurrent.STM.Stats

main :: IO ()
main = do
  let n = 10
  group <- new

  -- v <- newIORef ("a", 1)
  -- -- let run = randomSleep >> modifyIORef v update
  -- let run = do randomSleep
               -- (s, _) <- readIORef v
               -- randomSleep
               -- writeIORef v (s ++ "a", (length s) + 1)
  -- n `replicateM_` (G.forkIO group run)
  -- modifyIORef v update
  -- wait group
  -- (_, i) <- readIORef v
  -- putStrLn . ("IORef " ++) $ show i

  v <- trackSTM $ newTVar ("a", 1)
  -- let run = randomSleep >> atomically (modifyTVar v update)
  let run = do randomSleep
               trackNamedSTM "runner" $
                 do (s, _) <- readTVar v
                    unsafeIOToSTM randomSleep
                    writeTVar v (s ++ "a", (length s) + 1)

  n `replicateM_` (G.forkIO group run)
  -- atomically $ modifyTVar v update
  wait group
  (_, i) <- trackNamedSTM "reader" $ readTVar v
  putStrLn . ("TVar " ++) $ show i
  dumpSTMStats

  -- v <- atomically $ newTVar ("a", 1)
  -- -- let run = randomSleep >> atomically (modifyTVar v update)
  -- let run = atomically $
            -- do unsafeIOToSTM $ putStrLn "in here"
               -- (s, _) <- readTVar v
               -- unsafeIOToSTM $ putStrLn "and in here"
               -- writeTVar v (s ++ "a", (length s) + 1)

  -- n `replicateM_` (G.forkIO group run)
  -- -- atomically $ modifyTVar v update
  -- wait group
  -- (_, i) <- (atomically $ readTVar v)
  -- putStrLn . ("TVar " ++) $ show i

  -- v <- atomically $ newTMVar ("a", 1)
  -- let run = randomSleep >> atomically (modifyTMVar v update)
      -- modifyTMVar var f = do { val <- takeTMVar var; putTMVar var (f val)}
  -- n `replicateM_` (G.forkIO group run)
  -- atomically $ modifyTMVar v update
  -- wait group
  -- (_, i) <- (atomically $ readTMVar v)
  -- putStrLn . ("TMVar " ++) $ show i

  where randomSleep = randomRIO (1, 5) >>= sleep
        update (s, _) = (s ++ "a", (length s) + 1)
