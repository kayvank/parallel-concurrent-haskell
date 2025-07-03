{-# LANGUAGE RecordWildCards #-}

-- | Chapter 12, Chat client
module C12.ChatClient where

import Control.Concurrent.STM (STM, TChan, TVar, newTChan, newTVar, writeTChan)
import System.IO (Handle)

type ClientName = String

data Message
  = Notice String
  | Tell ClientName String
  | Broadcast ClientName String
  | Command String

data Client = Client
  { clientName :: ClientName
  , clientHandle :: Handle
  , clientKicked :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  }

newClient :: ClientName -> Handle -> STM Client
newClient clientName clientHandle = do
  clientKicked <- newTVar Nothing
  clientSendChan <- newTChan
  pure Client{..}

sendMessage :: Client -> Message -> STM ()
sendMessage Client{clientSendChan} =
  writeTChan clientSendChan
