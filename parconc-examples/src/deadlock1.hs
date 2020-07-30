import Control.Concurrent
import Control.Exception

-- <<main
main :: IO ()
main = do
  lock <- newEmptyMVar
  complete <- newEmptyMVar
  forkIO $ takeMVar lock `finally` putMVar complete ()
  takeMVar complete
-- >>
