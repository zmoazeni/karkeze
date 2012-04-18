import System.Posix.Signals
import Control.Concurrent
import Control.Concurrent.MVar

handler :: MVar Bool -> IO ()
handler keepRunning = do putStrLn "In handler"
                         swapMVar keepRunning False
                         return ()
  
keepRepeating :: MVar Bool -> IO ()
keepRepeating keepRunning = readMVar keepRunning >>= doRepeat
  where doRepeat shouldRun
          | shouldRun = do threadDelay 1000000
                           putStrLn "In here"
                           keepRepeating keepRunning
          | otherwise = return ()

main :: IO ()
main = do keepRunning <- newMVar True
          installHandler sigINT (Catch (handler keepRunning)) Nothing
          keepRepeating keepRunning
  

