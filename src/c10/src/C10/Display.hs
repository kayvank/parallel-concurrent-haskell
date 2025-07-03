{-# LANGUAGE LambdaCase #-}

-- | Chapter 10, Software Transaction Memory, STM
module C10.Display where

import Control.Concurrent (ThreadId, forkFinally)
import Control.Concurrent.STM (
  STM,
  TMVar,
  TVar,
  atomically,
  newEmptyTMVar,
  newEmptyTMVarIO,
  orElse,
  putTMVar,
  readTMVar,
  readTVar,
  retry,
  throwSTM,
  writeTVar,
 )
import Control.Exception (SomeException)
import Data.Map (Map, (!))
import Data.Set (Set)
import Data.Set qualified as Set

newtype Desktop = Desktop {did :: String} deriving (Show, Ord, Eq)
newtype Window = Window {wtitle :: String} deriving (Show, Ord, Eq)

type Display = Map Desktop (TVar (Set Window))
type UserFocus = TVar Desktop

moveWindow :: Display -> Window -> Desktop -> Desktop -> IO ()
moveWindow disp win a b = atomically $ moveWindowSTM disp win a b

moveWindowSTM :: Display -> Window -> Desktop -> Desktop -> STM ()
moveWindowSTM disp win a b = do
  let (ma, mb) = (disp ! a, disp ! b)
  wa <- readTVar ma
  wb <- readTVar mb
  writeTVar ma (Set.delete win wa)
  writeTVar mb (Set.insert win wb)

swapWindowSTM
  :: Display
  -> Window
  -> Desktop
  -> Window
  -> Desktop
  -> STM ()
swapWindowSTM disp win a v b = do
  moveWindowSTM disp win a b
  moveWindowSTM disp v b a

render :: Set Window -> IO ()
render _ = putStrLn "completed rending"

getWindows :: Display -> UserFocus -> STM (Set Window)
getWindows disp uf = do
  desktop <- readTVar uf
  readTVar (disp ! desktop)

renderThread :: Display -> UserFocus -> IO ()
renderThread disp focus = do
  wins <- atomically $ getWindows disp focus
  loop wins
  where
    loop wins = do
      render wins
      next <- atomically $ do
        wins' <- getWindows disp focus
        if wins' == wins
          then retry
          else pure wins'
      loop next

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  threadid <- forkFinally action (atomically . putTMVar var)
  pure (Async threadid var)

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var

waitSTM :: Async a -> STM a
waitSTM a =
  waitCatchSTM a >>= \case
    Left e -> throwSTM e
    Right b -> pure b

waithEither :: Async a -> Async b -> IO (Either a b)
waithEither a b =
  atomically $
    (Left <$> waitSTM a) `orElse` (Right <$> waitSTM b)

waitAny :: [Async a] -> IO a
waitAny = atomically . foldr (orElse . waitSTM) retry
