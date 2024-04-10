module Expression.Simplify ( simplify ) where

import Expression.Expression ( Expr(..) )
import Expression.BinOperator ( BinOperator(..) )

simplify :: (Num a, Ord a) => Expr a -> Expr a
simplify (Operation Addition x (Number 0)) = simplify x
simplify (Operation Addition (Number 0) x) = simplify x
simplify (Operation Subtraction x (Number 0)) = simplify x
simplify (Operation Multiplication x (Number 0)) = Number 0
simplify (Operation Multiplication (Number 0) x) = Number 0
simplify (Operation Multiplication x (Number 1)) = simplify x
simplify (Operation Multiplication (Number 1) x) = simplify x
simplify (Operation Exponentiation x (Number 1)) = simplify x
simplify (Operation Division x (Number 1)) = simplify x
simplify (SquareRoot (Number 0)) = Number 0
simplify (SquareRoot (Number 1)) = Number 1
simplify x = x