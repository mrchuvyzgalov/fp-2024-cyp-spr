module Expression.BinOperator where

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