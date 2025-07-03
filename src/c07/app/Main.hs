module Main where

import C07.Logger qualified
import Data.Maybe (isJust)
import System.Environment (getArgs)

main :: IO ()
main = do
  C07.Logger._main
  putStrLn "--End--"
