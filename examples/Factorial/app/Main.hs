{-|
Description: Several ways to define the n! function.
License    : MIT License
Stability  : demo
-}
module Main
    ( factorialRec
    , factorialRec2
    , factorialFold
    , factorialProd
    , main
    ) where

-- |Factorial function defined recursively.
factorialRec :: Integer -> Integer
factorialRec n = if n == 0 then 1 else n * factorialRec(n-1)

-- |Factorial function defined using pattern matching
factorialRec2 :: Integer -> Integer
factorialRec2 0 = 1
factorialRec2 n = n * factorialRec2(n-1)

-- |Factorial function defined using the predefined 'foldl' function.
factorialFold :: Integer -> Integer
factorialFold n = foldl(*) 1 [1..n]

-- |Factorial function defined using the predefined 'product' function.
factorialProd :: Integer -> Integer
factorialProd n = product [1..n]

-- |Program entry point which prints out results from calls to the above functions.
main :: IO ()
main = do
    let x = 5
    putStrLn ("factorialRec(" ++ (show x) ++ ") =" ++ (show (factorialRec x))) ;
    putStrLn ("factorialRec2(" ++ (show x) ++ ")=" ++ (show (factorialRec2 x))) ;
    putStrLn ("factorialFold(" ++ (show x) ++ ")=" ++ (show (factorialFold x))) ;
    putStrLn ("factorialProd(" ++ (show x) ++ ")=" ++ (show (factorialProd x))) ;
    return ()
