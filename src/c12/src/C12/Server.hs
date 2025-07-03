-- | Chapter12 Concurrent Network SErvers
module C12.Server where

import Control.Concurrent (forkFinally)
import Control.Monad (forever)
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
import Text.Printf (printf)

port :: Int
port = 4444

talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  loop
  where
    loop = do
      line <- hGetLine h
      if line == "end"
        then hPutStrLn h "Thank you using the Haskell doubling service."
        else hPrint h (2 * (read line :: Integer)) >> loop

_main :: IO ()
_main = withSocketsDo $ do
  listenOn port $ \sock ->
    forever $
      accept sock $ \(handle, peer) -> do
        printf "Accepted connectionf %s\n" (show peer)
        forkFinally (talk handle) (\_ -> hClose handle)
