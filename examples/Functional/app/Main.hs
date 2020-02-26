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
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs

main = do
    let xs = [1, 3, 5, 7, 9]
    putStr "input list: "
    print xs
    putStr "map(*2)   : "
    --print (map (\x -> 2 * x) xs)
    print (map1 (*2) xs)
    putStr "filter(<5): "
    print (filter1 (<5) xs)
