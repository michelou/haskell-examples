{-|
Description: Dealing with user inputs.
License    : MIT License
Stability  : demo
-}
module Main (main) where

import Control.Monad -- forM, mapM

<<<<<<< HEAD
-| Program entry point.
=======
-- |Program entry point.
>>>>>>> b2ef63c3dc3c051d1076d1df2506a0fac20aaca0
main :: IO ()
main = do
    colors <- forM [1 :: Int, 2, 3, 4] (\i -> do
        putStrLn $ "Which color do you associate with the number " ++ show i ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM_ putStrLn colors
