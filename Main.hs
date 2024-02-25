module Main where 

import Text.Printf (printf)
import Control.Monad (unless)
import Distribution.Simple.Utils (xargs)
import Data.Either (isLeft)

data BinOperator 
  = Addition
  | Subtraction 
  | Multiplication
  | Division
  | Exponentiation 
  deriving (Eq)

instance Show BinOperator where 
  show Addition = "+"
  show Subtraction = "-"
  show Multiplication = "*"
  show Division = "/"
  show Exponentiation = "^"

data Expr  
  = Number Double 
  | SquareRoot Expr
  | Operation BinOperator Expr Expr 

instance Show Expr where 
  show (Number x) = show x
  show (SquareRoot x) = "sqrt(" ++ show x ++ ")"
  show (Operation op x y) = "(" ++ show x ++ " " ++ show op ++ " " ++ show y ++ ")"

instance Eq Expr where 
  (==) (Number x) (Number y) = x == y 
  (==) (SquareRoot x) (SquareRoot y) = x == y
  (==) (Operation op x y) (Operation op' x' y') = op == op' && x == x' && y == y'
  (==) _ _ = False

data Error 
  = SquareRootError
  | DividingByZeroError

instance Show Error where 
  show SquareRootError = "You can't take the square root of a negative number"
  show DividingByZeroError = "You can't divide by zero"

instance Eq Error where 
  (==) SquareRootError SquareRootError = True
  (==) DividingByZeroError DividingByZeroError = True
  (==) _ _ = False 

extractValues :: Either Error Double -> Either Error Double -> Either Error (Double, Double)
extractValues (Right x) (Right y) = Right (x, y)
extractValues (Left x) (Right y) = Left x
extractValues _ (Left y) = Left y

takeOperation :: BinOperator -> Double -> Double -> Either Error Double
takeOperation Addition x y = Right (x + y)
takeOperation Subtraction x y = Right (x - y)
takeOperation Multiplication x y = Right (x * y)
takeOperation Exponentiation x y = Right (x ** y)
takeOperation Division x y 
  | y == 0 = Left DividingByZeroError
  | otherwise = Right (x / y)

eval :: Expr -> Either Error Double 
eval (Number x) = Right x
eval (SquareRoot x) = 
  case eval x of 
    Left error -> Left error
    Right evaledX -> if evaledX < 0 then Left SquareRootError else Right $ sqrt evaledX
eval (Operation op x y) =
  case extractValues (eval x) (eval y) of
    Left error -> Left error
    Right (evaledX, evaledY) -> takeOperation op evaledX evaledY

cases :: [(Expr, Either Error Double)]
cases = [
  (Number 5, Right 5), 
  (SquareRoot (Number 4), Right 2),
  (SquareRoot (Number (-2)), Left SquareRootError),
  (Operation Addition (Number 1) (Number 2), Right 3),
  (Operation Subtraction (Number 3) (Number 2), Right 1),
  (Operation Multiplication (Number 2) (Number 3), Right 6),
  (Operation Exponentiation (Number 2) (Number 3), Right 8),
  (Operation Division (Number 6) (Number 3), Right 2),
  (Operation Division (Number 6) (Number 0), Left DividingByZeroError)
  ] 

test :: Expr -> Either Error Double -> IO () 
test expr expected = 
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 
  

main :: IO () 
main = do 
  mapM_ (uncurry test) cases 
  