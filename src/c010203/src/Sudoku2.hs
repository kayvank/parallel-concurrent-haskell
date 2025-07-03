{-
ghc -O2 sudoku2.hs -rtsopts
-}
module Sudoku2 where

import Control.DeepSeq (force)
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Data.Maybe (isJust)
import Sudoku
import System.Environment (getArgs)

-- <<main
main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file

      (as, bs) = splitAt (length puzzles `div` 2) puzzles -- <1>
      solutions = runEval $ do
        as' <- rpar (force (map solve as)) -- <2>
        bs' <- rpar (force (map solve bs)) -- <2>
        rseq as' -- <3>
        rseq bs' -- <3>
        return (as' ++ bs') -- <4>
  print (length (filter isJust solutions))
