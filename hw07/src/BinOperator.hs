module BinOperator where

import Data.Map (Map)
import qualified Data.Map as Map

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

binOperatorMap :: Map Char BinOperator
binOperatorMap = Map.fromList [('+', Addition), ('-', Subtraction), ('*', Multiplication), ('/', Division), ('^', Exponentiation)]