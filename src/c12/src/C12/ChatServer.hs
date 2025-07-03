{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Chapter 12, ChatServer
module C12.ChatServer where

import C12.ChatClient (Client (..), ClientName, Message (..), newClient, sendMessage)
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (
  STM,
  TVar,
  atomically,
  newTVar,
  newTVarIO,
  readTChan,
  readTVar,
  readTVarIO,
  writeTChan,
  writeTVar,
 )
import Control.Exception (finally, mask)
import Control.Monad (forever, join, when)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map (Map)
import Data.Map qualified
import GHC.IO.Device (RawIO (write))
import Network.Socket (withSocketsDo)
import NetworkUtils (accept, listenOn)
import System.IO (
  BufferMode (LineBuffering),
  Handle,
  hClose,
  hGetLine,
  hPutStrLn,
  hSetBuffering,
  hSetNewlineMode,
  universalNewlineMode,
 )
import Text.Printf (hPrintf, printf)

newtype Server = Server
  { clients :: TVar (Map ClientName Client)
  }

newServer :: IO Server
newServer = Server <$> newTVarIO Data.Map.empty

broadcast :: Server -> Message -> STM ()
broadcast Server{clients} message = do
  clients' <- readTVar clients
  traverse_ f (Data.Map.elems clients')
  where
    f client = writeTChan (clientSendChan client) message

port :: Int
port = 4444

runServer :: IO ()
runServer = withSocketsDo $ do
  server <- newServer
  printf "Listening on port %d\n" port
  listenOn port $ \soc ->
    forever $ accept soc $ \(handle, peer) ->
      printf "Accepted connction from %s:\n" (show peer)
        >> forkFinally (talk handle server) (\_ -> hClose handle)

{- |
takes a username and attempts to add a new client with that name to the
state, returning Nothing if a client with that name already exists,
or Just client if the
-}
checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if Data.Map.member name clientmap
    then return Nothing
    else do
      client <- newClient name handle
      writeTVar clients $ Data.Map.insert name client clientmap
      broadcast server $ Notice (name ++ " has connected")
      return (Just client)

{- |
create the client threads themselves and start processing
events.
runClient returns or throws an exception only when the
client is to be disconnected
-}
runClient :: Server -> Client -> IO ()
runClient srvr client@Client{..} =
  let
    receive :: IO ()
    receive =
      forever $
        hGetLine clientHandle
          >>= atomically . sendMessage client . Command
    server :: IO ()
    server =
      readTVarIO clientKicked >>= \case
        Just reason ->
          hPutStrLn clientHandle $ "You have been kicked off: " <> reason
        Nothing ->
          atomically (readTChan clientSendChan)
            >>= handleMessage srvr client
            >>= flip when server
   in
    void $ race server receive

{- |
Indicate whether the caller should continue to
handle more messages (True) or exit (False).
-}
handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  let
    outPut s = hPutStrLn clientHandle s >> pure True
   in
    case message of
      Notice msg -> outPut $ "*** " <> msg
      Tell name msg -> outPut $ "* " <> name <> "*: " <> msg
      Broadcast name msg -> outPut $ "<" <> name <> ">: " <> msg
      Command msg -> case words msg of
        ["/kick", who] ->
          atomically (kick server who clientName)
            >> pure True
        ("/tell" : who : what) ->
          tell server client who (unwords what)
            >> pure True
        ["/quit"] ->
          pure False
        ('/' : _) : _ ->
          hPutStrLn clientHandle ("Unrecognized command: " <> msg)
            >> pure True
        _ ->
          atomically (broadcast server (Broadcast clientName msg))
            >> pure True

removeClient :: Server -> ClientName -> IO ()
removeClient Server{clients} name =
  atomically $
    readTVar clients >>= writeTVar clients . Data.Map.delete name

kick :: Server -> ClientName -> ClientName -> STM ()
kick srvr@Server{..} who by = do
  cs <- readTVar clients
  case Data.Map.lookup who cs of
    Just victim -> do
      writeTVar clients (Data.Map.delete (clientName victim) cs)
      void $ sendToName srvr by (Notice $ "you kicked " <> clientName victim)
    Nothing ->
      void $ sendToName srvr by (Notice $ who <> " is not connected")

sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName Server{..} name msg = do
  clients' <- readTVar clients
  case Data.Map.lookup name clients' of
    Just target -> sendMessage target msg >> pure True
    Nothing -> pure False

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server Client{..} who str = do
  atomically (sendToName server who (Tell clientName str)) >>= \case
    True -> pure ()
    False -> hPutStrLn clientHandle (who ++ " is not connected.")

talk :: Handle -> Server -> IO ()
talk handle server = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  readName
  where
    readName = do
      hPutStrLn handle "What is your name?"
      name <- hGetLine handle
      if null name
        then readName
        else mask $ \restore -> do
          ok <- checkAddClient server name handle
          case ok of
            Nothing -> restore $ do
              hPrintf handle "The name %s is in use, please chose another \n"
            Just client ->
              restore (runClient server client)
                `finally` removeClient server name
