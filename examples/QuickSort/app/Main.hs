{-|
Description: Different implementations of the Quicksort algorihtm.
License    : MIT License
-}
module Main (main) where

-- | QuickSort function using the function `filter` from module `GHC.List`.
--   (see https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.List.html) 
qsort :: [Int] -> [Int]
qsort []      = []
qsort (a:as)  = qsort left ++ [a] ++ qsort right
    where (left, right) = (filter (<=a) as, filter (>a) as)

-- | QuickSort function using a user-defined filter function.
qsort1 :: [Int] -> [Int]
qsort1 []     = []
qsort1 (x:xs) = qsort [y | y <- xs, y < x]
              ++ [x]
              ++ qsort [ y | y <- xs, y >= x]

-- |Program entry point.
main :: IO ()
main = do
    let xs = [8, 4, 0, 3, 1, 23, 11, 18]
    putStr "input list       : "
    print xs
    putStr "sorted(filter)   : "
    print (qsort xs)
    putStr "sorted(list comp): "
    print (qsort1 xs)
