module C07.Mvar where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)

_main :: IO ()
_main = do
  m <- newEmptyMVar
  void $ forkIO $ putMVar m 'x' >> putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
