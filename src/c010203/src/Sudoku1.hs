{- |
<<all
-}
module Sudoku1 where

import Control.Exception
import Data.Maybe
import Sudoku
import System.Environment

main :: IO ()
main = do
  [f] <- getArgs -- <1>
  file <- readFile f -- <2>
  let puzzles = lines file -- <3>
      solutions = map solve puzzles -- <4>
  print (length (filter isJust solutions)) -- <5>
