{-|
Description: Evaluating higher-order functions.
License    : MIT License
Stability  : demo
-}
module Main (main) where

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 p (x:xs)
    | p x = x : filter1 p xs
    | otherwise = filter1 p xs

map1 :: (a -> b) -> [a] -> [b]
map1 _ [] = []
map1 f (x:xs) = f x : map1 f xs

-- |Program entry point.
main :: IO ()
main = do
    let xs = [1, 3, 5, 7, 9] :: [Int]
    putStr ("             xs: " ++ (show xs) ++ "\n") ;
    putStr ("map     (*2) xs: " ++ (show (map (\x -> 2 * x) xs)) ++ "\n") ;
    putStr ("map1    (*2) xs: " ++ (show (map1 (*2) xs)) ++ "\n") ;
    putStr ("filter  (<5) xs: " ++ (show (filter (<5) xs)) ++ "\n") ;
    putStr ("filter1 (<5) xs: " ++ (show (filter1 (<5) xs)) ++ "\n") ;
    return ()
