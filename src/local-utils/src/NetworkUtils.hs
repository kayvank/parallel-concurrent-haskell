module NetworkUtils where

import Control.Exception (bracket, bracketOnError)
import Data.List.NonEmpty qualified as NE
import Network.Run.TCP (openTCPServerSocket, resolve)
import Network.Socket (
  AddrInfoFlag (AI_PASSIVE),
  SockAddr,
  Socket,
  SocketType (Stream),
  close,
  socketToHandle,
 )
import Network.Socket qualified as Socket
import System.IO (Handle, IOMode (ReadWriteMode))

listenOn :: Int -> (Socket -> IO a) -> IO a
listenOn port server = do
  addr <- resolve Stream Nothing (show port) [AI_PASSIVE] -- NE.head
  bracket (openTCPServerSocket addr) close server

accept :: Socket -> ((Handle, SockAddr) -> IO a) -> IO a
accept sock server = do
  bracketOnError (Socket.accept sock) (close . fst) $
    \(conn, peer) -> do
      handle <- socketToHandle conn ReadWriteMode
      server (handle, peer)
