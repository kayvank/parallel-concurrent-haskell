module Main where

import C12.ChatServer qualified
import C12.STMServer qualified
import C12.Server qualified

main :: IO ()
main = do
  C12.ChatServer.runServer
  putStrLn "--End--"
