module Timeit (
  timeit,
)
where

import Data.Time (diffUTCTime, getCurrentTime)

timeit :: IO a -> IO (a, Double)
timeit action = do
  t0 <- getCurrentTime
  a <- action
  tf <- getCurrentTime
  pure (a, realToFrac $ tf `diffUTCTime` t0)
