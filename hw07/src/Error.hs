module Error where

data Error 
  = SquareRootError
  | DividingByZeroError
  | UnknownVariableError String
  deriving (Eq)

instance Show Error where 
  show SquareRootError = "You can't take the square root of a negative number"
  show DividingByZeroError = "You can't divide by zero"
  show (UnknownVariableError var) = "Variable " ++ show var ++ " is unknown"