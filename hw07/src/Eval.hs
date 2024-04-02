module Eval ( eval, runEval, ExprState ) where

import Error ( Error(..) )
import Expression ( Expr(..) )
import BinOperator ( BinOperator(..) )
import StateDemo ( State(..), get, execState )

import qualified Data.Map.Strict as M

type ExprState a = [(String, a)]

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

eval :: (Floating a, Ord a) => Expr a -> State (ExprState a) (Either Error a)
eval (Var name) = 
  do
    env <- get
    case lookup name env of
      Just x -> return $ Right x
      Nothing -> return $ Left $ UnknownVariableError name
eval (Number x) = return $ Right x
eval (SquareRoot x) =
  do
    expr <- eval x
    case expr of
      Right evaledX -> if evaledX < 0 then return $ Left SquareRootError else return $ Right $ sqrt evaledX
      Left error -> return $ Left error
eval (Operation op x y) = 
  do
    x' <- eval x
    y' <- eval y
    case extractValues x' y' of
      Right (evaledX, evaledY) -> return $ takeOperation op evaledX evaledY
      Left error -> return $ Left error

runEval :: (Floating a, Ord a) => Expr a -> ExprState a -> Either Error a
runEval expr = execState (eval expr)