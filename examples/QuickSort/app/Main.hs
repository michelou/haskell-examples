{-|
Description: Different implementations of the Quicksort algorihtm.
License    : MIT License
-}
module Main (main) where

import Control.Monad.ST (runST, ST)
import Data.Array.ST (getElems, newListArray, readArray, STArray, writeArray)

-- | QuickSort function using the function `filter` from module `GHC.List`.
--   (see https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.List.html) 
qsort :: [Int] -> [Int]
qsort []      = []
qsort (x:xs)  = qsort left ++ [x] ++ qsort right
    where (left, right) = (filter (<=x) xs, filter (>x) xs)

-- | QuickSort function using a user-defined filter function.
qsort1 :: [Int] -> [Int]
qsort1 []     = []
qsort1 (x:xs) = qsort smaller ++ [x] ++ qsort greater
    where smaller = [y | y <- xs, y < x]; greater = [ y | y <- xs, y >= x]

-- | QuickSort function using the ST monad (https://wiki.haskell.org/Monad/ST).
qsort2 :: Ord a => [a] -> [a]
qsort2 xs = runST $ do {
    xa <- newListArray (0, n-1) xs;
    qsortST xa (0, n);
    getElems xa
} where n = length xs

qsortST :: Ord a => STArray s Int a -> (Int, Int) -> ST s ()
qsortST xa (a, b)
    | a == b =
        return ()
    | otherwise =
        do { m <- partition xa (a, b);
             qsortST xa (a, m);
             qsortST xa (m+1, b)
        }

partition :: Ord a => STArray s Int a -> (Int, Int) -> ST s Int
partition xs (a, b) = do {
    x <- readArray xs a;
    let loop (j, k) = if j==k then do {
        swap xs a (k-1);
        return (k-1)
    } else do {
        y <- readArray xs j;
        if y < x then loop (j+1, k)
        else do {
            swap xs j (k-1);
            loop (j, k-1)
        }
    }
    in loop (a+1, b)
}

swap :: STArray s Int a -> Int -> Int -> ST s ()
swap xa i j = do {
    v <- readArray xa i;
    w <- readArray xa j;
    writeArray xa i w;
    writeArray xa j v
}

-- | Program entry point.
main :: IO ()
main = do
    let xs = [8, 4, 0, 3, 1, 23, 11, 18]
    putStr "input list       : "
    print xs
    putStr "sorted(filter)   : "
    print (qsort xs)
    putStr "sorted(list comp): "
    print (qsort1 xs)
    putStr "sorted(ST)       : "
    print (qsort2 xs)
