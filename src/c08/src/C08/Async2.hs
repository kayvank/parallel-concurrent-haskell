{-# LANGUAGE LambdaCase #-}

module C08.Async2 where

import Control.Concurrent (
  MVar,
  forkIO,
  newEmptyMVar,
  newMVar,
  putMVar,
  readMVar,
  takeMVar,
  threadDelay,
 )
import Control.Exception (SomeException, throw, throwIO, try)
import Data.ByteString qualified as B
import Data.ByteString.Char8 (pack)
import Data.Functor (void)
import Data.String qualified as B
import GHC.Exception (ErrorCall (..))

getURL2 :: String -> IO B.ByteString
getURL2 s = do
  threadDelay 1000_000
  if even (length s)
    then
      pure $ throw $ ErrorCall "url not reachable"
    else pure (B.fromString $ s <> " End download")

getURL :: String -> IO B.ByteString
getURL s =
  threadDelay 1000_000 >> pure (B.fromString $ s <> " End download")

data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  v <- newEmptyMVar
  void $ forkIO $ try action >>= putMVar v
  pure $ Async v

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async v) = readMVar v

wait :: Async a -> IO a
wait as =
  waitCatch as >>= \case
    Left e -> throwIO e
    Right a -> pure a

_main1 :: IO ()
_main1 = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar

  void $
    forkIO $
      getURL "www1" >>= putMVar m1

  void $
    forkIO $
      getURL "www2" >>= putMVar m2

  r1 <- takeMVar m1
  r2 <- takeMVar m2

  print (B.length r1, B.length r2)

_main2 :: IO ()
_main2 = do
  _1 <- async $ getURL "www1"
  _2 <- async $ getURL "wwww2"
  w1 <- wait _1
  w2 <- wait _2
  print (B.length w1, B.length w2)

_main :: IO ()
_main = do
  ws <- mapM (async . getURL2) ["ww0", "ww1", "wws2"]
  results <- mapM wait ws
  mapM_ print results
