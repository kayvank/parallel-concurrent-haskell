module Main where

import C12.STMServer qualified
import C12.Server qualified

main :: IO ()
main = do
  C12.STMServer._main
  putStrLn "--End--"
