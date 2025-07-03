module Main where

import C09.AsyncException qualified
import Data.Maybe (isJust)
import System.Environment (getArgs)

main :: IO ()
main = do
  C09.AsyncException._main
  putStrLn "--End--"
