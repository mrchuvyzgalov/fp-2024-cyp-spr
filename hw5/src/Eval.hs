module Eval ( eval ) where 

import Error ( Error(..) )
import Expression ( Expr(..) )
import BinOperator ( BinOperator(..) )

import qualified Data.Map.Strict as M 

extractValues :: Either Error a -> Either Error a -> Either Error (a, a)
extractValues (Right x) (Right y) = Right (x, y)
extractValues (Left x) _ = Left x
extractValues _ (Left y) = Left y

takeOperation :: (Floating a, Ord a) => BinOperator -> a -> a -> Either Error a
takeOperation Addition x y = Right (x + y)
takeOperation Subtraction x y = Right (x - y)
takeOperation Multiplication x y = Right (x * y)
takeOperation Exponentiation x y = Right (x ** y)
takeOperation Division x y 
  | y == 0 = Left DividingByZeroError
  | otherwise = Right (x / y)

eval :: (Floating a, Ord a) => Expr a -> M.Map String a -> Either Error a
eval (Var name) vars = 
  case M.lookup name vars of
    Just value -> Right value
    Nothing -> Left (UnknownVariableError name)
eval (Number x) _ = Right x
eval (SquareRoot x) vars = 
  case eval x vars of 
    Right evaledX -> if evaledX < 0 then Left SquareRootError else Right $ sqrt evaledX
    error -> error
eval (Operation op x y) vars =
  case extractValues (eval x vars) (eval y vars) of
    Right (evaledX, evaledY) -> takeOperation op evaledX evaledY
    Left error -> Left error