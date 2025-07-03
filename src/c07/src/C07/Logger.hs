{-# LANGUAGE LambdaCase #-}

module C07.Logger where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Functor ((<&>))

data LogCommand
  = Message String
  | Stop (MVar ())

data Logger = Logger (MVar LogCommand)

initLogger :: IO Logger
initLogger = do
  l <- newEmptyMVar <&> Logger
  void $ forkIO (logger l)
  pure l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      takeMVar m >>= \case
        Message s -> putStrLn s >> loop
        Stop m' -> putStrLn "logger: stop" >> putMVar m' ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) = putMVar m . Message

logStop :: Logger -> IO ()
logStop (Logger m) = do
  m' <- newEmptyMVar
  putMVar m (Stop m')
  takeMVar m'

_main :: IO ()
_main = do
  logger <- initLogger
  logMessage logger "hello"
  logMessage logger "bye"
  logStop logger
