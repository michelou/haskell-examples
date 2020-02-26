{-|
Description: Different implementations of the Quicksort algorihtm.
License    : MIT License
Stability  : demo
-}
module Main (main) where

qsort :: [Int] -> [Int]
qsort []      = []
qsort (a:as)  = qsort left ++ [a] ++ qsort right
    where (left,right) = (filter (<=a) as, filter (>a) as)

qsort1 :: [Int] -> [Int]
qsort1 []     = []
qsort1 (x:xs) = qsort [y | y <- xs, y < x]
              ++ [x]
              ++ qsort [ y | y <- xs, y >= x]

main :: IO ()
main = do
    let xs = [8, 4, 0, 3, 1, 23, 11, 18]
    putStr "input list       : "
    print xs
    putStr "sorted(filter)   : "
    print (qsort xs)
    putStr "sorted(list comp): "
    print (qsort1 xs)
