module Expression.Expression where

import Expression.BinOperator ( BinOperator(..) )
import Expression.Error ( Error(..) )

data Expr a
  = Number a
  | SquareRoot (Expr a)
  | Operation BinOperator (Expr a) (Expr a) 
  | Var String
  deriving (Eq)

instance Num a => Num (Expr a) where
    (+) x y = Operation Addition x y
    (-) x y = Operation Subtraction x y
    (*) x y = Operation Multiplication x y
    fromInteger = Number . fromInteger

instance Show a => Show (Expr a) where 
  show (Number x) = show x
  show (SquareRoot x) = "sqrt(" ++ show x ++ ")"
  show (Operation op x y) = "(" ++ show x ++ " " ++ show op ++ " " ++ show y ++ ")"
  show (Var x) = x

showExprInPrefixNotation :: (Show a) => Expr a -> String
showExprInPrefixNotation (Number x) = show x
showExprInPrefixNotation (SquareRoot expr) = "sqrt " ++ showExprInPrefixNotation expr
showExprInPrefixNotation (Operation Addition expr expr') = "+ " ++ showExprInPrefixNotation expr ++ " " ++ showExprInPrefixNotation expr'
showExprInPrefixNotation (Operation Subtraction expr expr') = "- " ++ showExprInPrefixNotation expr ++ " " ++ showExprInPrefixNotation expr'
showExprInPrefixNotation (Operation Multiplication expr expr') = "* " ++ showExprInPrefixNotation expr ++ " " ++ showExprInPrefixNotation expr'
showExprInPrefixNotation (Operation Division expr expr') = "/ " ++ showExprInPrefixNotation expr ++ " " ++ showExprInPrefixNotation expr'
showExprInPrefixNotation (Operation Exponentiation expr expr') = "^ " ++ showExprInPrefixNotation expr ++ " " ++ showExprInPrefixNotation expr'
showExprInPrefixNotation (Var name) = name
