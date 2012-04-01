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
  let n = 100
  group <- new

  v <- newIORef ("0", fromIntegral 0)
  -- let run = randomSleep >> modifyIORef v update
  let run = do yield
               val <- readIORef v
               -- randomSleep
               yield
               writeIORef v (update val)
  n `replicateM_` (G.forkIO group run)
  -- modifyIORef v update
  wait group
  (i, _) <- readIORef v
  putStrLn . ("IORef " ++) $ show i

  -- v <- trackSTM $ newTVar ("0", fromIntegral 0)
  -- -- let run = randomSleep >> atomically (modifyTVar v update)
  -- let run = do randomSleep
               -- trackNamedSTM "runner" $
                 -- do unsafeIOToSTM yield
                    -- val <- readTVar v
                    -- unsafeIOToSTM yield
                    -- writeTVar v (update val)

  -- n `replicateM_` (G.forkIO group run)
  -- -- atomically $ modifyTVar v update
  -- wait group
  -- (i, _) <- trackNamedSTM "reader" $ readTVar v
  -- putStrLn . ("TVar " ++) $ show i
  -- dumpSTMStats

  v <- trackSTM $ newTVar ("0", fromIntegral 0)
  -- let run = randomSleep >> atomically (modifyTVar v update)
  let run = do randomSleep
               val <- trackNamedSTM "getter" $ readTVar v
               yield
               trackNamedSTM "setter" $ writeTVar v (update val)

  n `replicateM_` (G.forkIO group run)
  -- atomically $ modifyTVar v update
  wait group
  (i, _) <- trackNamedSTM "reader" $ readTVar v
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

  v <- trackSTM $ newTMVar ("0", fromIntegral 0)
  let run = do randomSleep
               val <- trackNamedSTM "getter" $ takeTMVar v
               yield
               trackNamedSTM "setter" $ putTMVar v (update val)

  n `replicateM_` (G.forkIO group run)
  -- atomically $ modifyTMVar v update
  wait group
  (i, _) <- trackNamedSTM "reader" $ readTMVar v
  putStrLn . ("TMVar " ++) $ show i
  dumpSTMStats

  -- v <- atomically $ newTMVar ("0", fromIntegral 0)
  -- let run = do randomSleep
               -- val <- atomically $ takeTMVar v
               -- yield
               -- atomically $ putTMVar v (update val)

  -- n `replicateM_` (G.forkIO group run)
  -- -- atomically $ modifyTMVar v update
  -- wait group
  -- (i, _) <- (atomically $ readTMVar v)
  -- putStrLn . ("TMVar " ++) $ show i

  where sleepOne = sleep 1
        randomSleep = randomRIO (1, 5) >>= sleep
        update (s, _) = let i = (read s) :: Int; x = fromIntegral(i * 1000); y = fromIntegral(i * 2000) in (show (i + 1), calc x y)
        -- calc :: (Num a, Floating a) => a -> a -> a
        calc x y = sqrt(x*x+y*y)+3*cos(sqrt(x*x+y*y))+5
