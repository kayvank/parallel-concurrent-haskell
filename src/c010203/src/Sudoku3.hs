module Sudoku3 where

import Control.Exception
import Control.Parallel.Strategies hiding (parMap)
import Data.Maybe
import Sudoku
import System.Environment

-- <<main
main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
      solutions = runEval (parMap solve puzzles)

  print (length (filter isJust solutions))

-- >>

-- <<parMap
parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a : as) = do
  b <- rpar (f a)
  bs <- parMap f as
  return (b : bs)

-- >>
