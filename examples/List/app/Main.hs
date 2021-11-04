{-|
Description: List operations.
License    : MIT License
See        : https://kowainik.github.io/posts/hacktoberfest2020
-}
module Main (main) where

-- | Implement the subList function that crops list elements starting from
-- | a given index and ending under the given index, returning an empty list
-- | on negative indexes.
subList :: Int -> Int -> [a] -> [a]
subList from to list
    | from < 0 || to < 0 || to < from = []
    -- | otherwise = take (to - from + 1) (drop from list)
    | otherwise = drop from (take (to + 1) list)

-- | Alternate implementation
subList1 :: Int -> Int -> [a] -> [a]
subList1 start end list =
    if start > end || start < 0 || end < 0
        then []
        else take (end - start + 1) (drop start list)
                            
-- | Program entry point.
main :: IO ()
main = do
    let xs = [1::Int, 2, 3, 4, 5, 6, 7]
    putStrLn $ "xs             =" ++ show xs
    putStrLn $ "subList 1 3 xs =" ++ show (subList 1 3 xs)
    putStrLn $ "subList1 1 3 xs=" ++ show (subList1 1 3 xs)
    return ()
