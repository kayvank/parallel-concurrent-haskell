module C07.PhoneBook where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)

newtype Name = Name {unName :: String}
  deriving (Show, Eq, Ord)
newtype PhoneNumber = PhoneNumber {unNumber :: String}
  deriving (Show, Eq, Ord)
newtype PhoneBook = PhoneBook (Map Name PhoneNumber)
  deriving (Show, Eq, Ord)

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = newMVar (PhoneBook Map.empty) <&> PhoneBookState

lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  (PhoneBook phonebook) <- takeMVar m
  pure $ Map.lookup name phonebook

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState st) name phone = do
  PhoneBook phonebook <- takeMVar st -- locks against concurrent updates
  {-
  Here potentially MVar can build large thunk of unevaluted expressions,
  which could create space leak. We have 2 options:
  -1 force eager evaluation as we have here. The draw back is,
     that now we lock for the duration map insert. Not ideal
  -2 insert into map lazyly, release the lock, force the evaluation outside of mvar

  -}
  -- we need to make the operation eager to avoid space leak in highly concurrent apps
  -- putMVar st (PhoneBook $! Map.insert name phone phonebook) -- simultaneously unclocks and updates
  let book' = Map.insert name phone phonebook
  putMVar st (PhoneBook book')
  seq book' (pure ())
