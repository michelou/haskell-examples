{-|
Description: Dealing with user inputs.
License    : MIT License
-}
module Main (main) where

import Control.Monad -- forM, mapM

-- | Program entry point.
main :: IO ()
main = do
    colors <- forM [1 :: Int, 2, 3, 4] (\i -> do
        putStrLn $ "Which color do you associate with the number " ++ show i ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM_ putStrLn colors
