{-|
Description: Several ways to define the program entry point.
License    : MIT License
-}
module Main (main) where

-- |Three ways to define the main entry point which prints out "Hello world!".
-- 
-- > main = putStrLn "Hello world!"
--
-- > main = do {
-- >    putStrLn "Hello world!" ;
-- >    return ()
-- > }
--
-- > main = do
-- >    putStrLn "Hello world!"
-- >    return ()
main :: IO ()
main = do
    putStrLn "Hello world!"
    return ()
