module ThunkDemo where

import Data.Tuple (swap)

x = 1 + 2 :: Int
y = x + 1 :: Int
z = (x, y) :: (Int, Int)
x' = 10 + 20
z' = swap (x', x' + 42) :: (Int, Int)
xs = map (+ 1) [1 .. 10] :: [Int]

{-
:sprint x
:sprint y
:sprint z
:sprint z'
seq y ()

:sprint xs
seq xs ()
:sprint xs
length xs
:sprint xs
sum xs
:sprint xs
-}
