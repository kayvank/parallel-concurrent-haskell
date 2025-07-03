{-# LANGUAGE RecordWildCards #-}

{- |
Chapther 7 Sample code of `Parallel and Concurrent Programming in Haskell`
-}
module C07.Channel where

import Control.Concurrent (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)

newtype Stream a = Stream (MVar (Item a))
data Item a = Item a (Stream a)

data Channel a
  = Channel
  { readptr :: MVar (Stream a)
  , writeptr :: MVar (Stream a)
  }

newChannel :: IO (Channel a)
newChannel = do
  hole <- newEmptyMVar
  r :: MVar (Stream a) <- newMVar (Stream hole)
  w <- newMVar (Stream hole)
  pure $ Channel r w

writeChannel :: Channel a -> a -> IO ()
writeChannel Channel{writeptr} val = do
  newHole <- Stream <$> newEmptyMVar
  (Stream oldHole) <- takeMVar writeptr
  putMVar oldHole (Item val newHole)
  putMVar writeptr newHole

readChannel :: Channel a -> IO a
readChannel Channel{readptr} = do
  (Stream m) <- takeMVar readptr
  (Item val tail) <- readMVar m
  putMVar readptr tail
  pure val

dupChannel :: Channel a -> IO (Channel a)
dupChannel Channel{writeptr} = do
  hole <- readMVar writeptr
  readptr <- newMVar hole
  pure Channel{..}
