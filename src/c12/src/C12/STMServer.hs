module C12.STMServer where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (
  TChan,
  TVar,
  atomically,
  newTChanIO,
  newTVarIO,
  readTChan,
  readTVarIO,
  writeTChan,
  writeTVar,
 )
import Control.Monad (forever)
import Data.Functor (void)
import Network.Socket (withSocketsDo)
import NetworkUtils (accept, listenOn)
import System.IO (
  BufferMode (LineBuffering),
  Handle,
  hClose,
  hGetLine,
  hPrint,
  hPutStrLn,
  hSetBuffering,
 )
import Text.Printf (hPrintf, printf)

newtype State = State {currentFactor :: TVar Int}

talk :: Handle -> TVar Integer -> IO ()
talk h factor = do
  hSetBuffering h LineBuffering
  c <- newTChanIO
  void $ race (server h factor c) (recive h c)
  pure ()

recive :: Handle -> TChan String -> IO ()
recive h c = forever $ hGetLine h >>= atomically . writeTChan c

server :: Handle -> TVar Integer -> TChan String -> IO ()
server h factor c = do
  f <- readTVarIO factor
  hPrintf h "Current factor: %d\n" f
  loop f
  where
    loop :: Integer -> IO ()
    loop f = do
      f' <- readTVarIO factor
      if f /= f'
        then newfactor f'
        else do
          l <- atomically $ readTChan c
          command f l

    command :: Integer -> String -> IO ()
    command f s = case s of
      "end" -> hPutStrLn h "Thank you for using the haskell multiplyer server"
      '*' : s -> atomically (writeTVar factor (read s :: Integer)) >> loop f
      line -> hPrint h (f * (read line :: Integer)) >> loop f

    newfactor :: Integer -> IO ()
    newfactor f = hPrintf h "new factor: %d\n" f >> loop f
port :: Int
port = 44444
_main :: IO ()
_main = withSocketsDo $ do
  factor <- newTVarIO 2
  printf "Listening on port %d\n" port
  listenOn port $ \sock ->
    forever $
      accept sock $ \(handle, peer) ->
        printf "Accepted connction from %s:\n" (show peer)
          >> forkFinally (talk handle factor) (\_ -> hClose handle)
