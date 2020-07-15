{-|
Description: Evaluating arithmetic expressions.
License    : MIT License
Stability  : demo
-}
module Main (main) where

data Op   = Plus | Minus | Times | Divide

data Expr = Lit Integer
          | Bin Op Expr Expr

-- |8 - 2 * 5
expr1 :: Expr
expr1 = Bin Minus (Lit 8) (Bin Times (Lit 2) (Lit 5))

-- |Transform symbolic operator `Op` to real operator
evalOp :: Op -> (Integer -> Integer -> Integer)
evalOp Plus   = (+)
evalOp Minus  = (-)
evalOp Times  = (*)
evalOp Divide = div

eval :: Expr -> Integer
eval (Lit i)        = i
eval (Bin op e1 e2) = evalOp op (eval e1) (eval e2)

data Error a = Exception String | Result a

evalOpE :: Op -> (Integer -> Integer -> Error Integer)
evalOpE Divide  x 0 = Exception (show x ++ " / 0")
evalOpE op      x y = Result (evalOp op x y)

evalE :: Expr -> Error Integer
evalE (Lit i)        = Result i
evalE (Bin op e1 e2) = evalOpE op (eval e1)(eval e2)

instance Show (Error a) where
    show (Exception e) = e
    -- show (Result a) = "a"  -- TODO: What instead of "a" ?!
    show (Result _) = show (1 :: Integer)
    -- show (Result a) = show a -- No instance for (Show a) arising from a use of `show'

main :: IO ()
main = do
    let exprString = "8 - 2 * 5 = "
    putStr exprString
    print (eval expr1)
    putStr exprString
    print (evalE expr1)
