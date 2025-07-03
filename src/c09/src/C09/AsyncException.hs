{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

{- |
Chapter 9, Async Exceptions and Cancellation
-}
module C09.AsyncException where

import Control.Concurrent (
  MVar,
  ThreadId,
  forkIO,
  modifyMVar,
  modifyMVar_,
  newEmptyMVar,
  newMVar,
  putMVar,
  readMVar,
  takeMVar,
  threadDelay,
  throwTo,
 )
import Control.Exception (AsyncException (ThreadKilled), SomeException, throw, throwIO, try)
import Control.Monad (forever, when)
import Data.ByteString qualified as B
import Data.ByteString.Char8 (pack)
import Data.Either (rights)
import Data.Functor (void)
import Data.String qualified as B
import GetURL (getURL)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin)
import System.Timeout (timeout)
import Text.Printf (printf)
import Timeit (timeit)

timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeit (getURL url)
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  v <- newEmptyMVar
  threadId <- forkIO $ try action >>= putMVar v
  pure $ Async threadId v

cancel :: Async a -> IO ()
cancel (Async threadId _) = throwTo threadId ThreadKilled

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ v) = readMVar v

wait :: Async a -> IO a
wait as =
  waitCatch as >>= \case
    Left e -> throwIO e
    Right a -> pure a

casMVar :: (Eq a) => MVar a -> a -> a -> IO Bool
casMVar m old new = do
  modifyMVar m $ \cur ->
    if cur == old
      then pure (new, True)
      else pure (old, False)

modifyTwo :: MVar a -> MVar b -> (a -> b -> IO (a, b)) -> IO ()
modifyTwo ma mb f =
  modifyMVar_ mb $ \b ->
    modifyMVar ma $ \a -> f a b

-- timeout :: Int -> IO a -> IO (Maybe a)
-- timeout t action
--   | t == 0 = pure Nothing
--   | t < 0 = Just <$> action
--   | otherwise = undefined

sites :: [String]
sites =
  [ "http://www.google.com"
  , "http://www.bing.com"
  , "http://www.yahoo.com"
  , "http://www.wikipedia.com/wiki/Spade"
  , "http://www.wikipedia.com/wiki/Shovel"
  ]
_main :: IO ()
_main = do
  ws <- mapM (async . timeDownload) sites
  forkIO $ do
    hSetBuffering stdin NoBuffering
    forever $ do
      c <- getChar
      when (c == 'q') $ mapM_ cancel ws
  rs <- mapM waitCatch ws
  printf "%d/%d succeeded\n" (length (rights rs)) (length rs)

expensiveComputation :: a -> IO a
expensiveComputation a =
  threadDelay 9_000_000 >> putStrLn "in expensiveComputation" >> pure a

mainWithTimeout :: IO ()
mainWithTimeout = do
  async $
    timeout 8_000_000 (expensiveComputation "some-test-string") >>= \case
      Just a -> putStrLn $ "returned " <> show a <> " finished ontime"
      Nothing -> putStrLn "DID not  finish ontime!!"
  forever $ do
    putStrLn "enter a line "
    c <- getLine
    putStrLn $ "you entered: " <> show c
