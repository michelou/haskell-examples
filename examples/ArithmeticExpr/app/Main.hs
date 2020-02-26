{-|
Description: Evaluating arithmetic expressions.
License    : MIT License
Stability  : demo
-}
module Main (main) where

data Op   = Plus | Minus | Times | Divide

data Expr = Lit Integer
          | Bin Op Expr Expr

-- 8 - 2 * 5
expr1 :: Expr
expr1 = Bin Minus (Lit 8) (Bin Times (Lit 2) (Lit 5))

evalOp :: Op -> (Integer -> Integer -> Integer)
evalOp Plus   = (+)
evalOp Minus  = (-)
evalOp Times  = (*)
evalOp Divide = div

eval :: Expr -> Integer
eval (Lit i)        = i
eval (Bin op e1 e2) = (evalOp op) (eval e1) (eval e2)

data Error a = Exception String | Result a

evalOpE :: Op -> (Integer -> Integer -> Error Integer)
evalOpE Divide  x 0 = Exception (show x ++ " / 0")
evalOpE op      x y = Result (evalOp op x y)

evalE :: Expr -> Error Integer
evalE (Lit i)        = Result i
evalE (Bin op e1 e2) = do
    x1 <- evalE e1
    x2 <- evalE e2
    evalOpE op x1 x2

instance Show (Error a) where
    show a = "Error "

main :: IO ()
main = do
    putStr "8 - 2 * 5 = "
    print (eval expr1)
    let res = evalE expr1
    print res
