module Rpar where

import Control.Parallel.Strategies (Strategy, rpar, runEval)

parPair :: Strategy (a, b)
parPair (a, b) = do
  a' <- rpar a
  b' <- rpar b
  return (a', b')

-- | Parameterized Strategy
evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = do
  a' <- sa a
  b' <- sb b
  return (a', b')

{-
runEval (rparPari (fib 35, fib 36))
-}
using :: a -> Strategy a -> a
x `using` s = runEval $ s x
