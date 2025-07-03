module Main where

import C08.Async2 qualified
import Data.Maybe (isJust)
import System.Environment (getArgs)

main :: IO ()
main = do
  C08.Async2._main
  putStrLn "--End--"
