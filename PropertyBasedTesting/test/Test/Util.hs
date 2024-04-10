module Test.Util where

import Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Expression.BinOperator
import Expression.Expression 
import Expression.Eval

genDouble :: Int -> Gen Double
genDouble n = fromIntegral <$> Gen.int (Range.constant 0 n) 

genInt :: Int -> Gen Int
genInt n = Gen.int (Range.constant 0 n) 

getVarList :: [String]
getVarList = ["x", "y", "z"]

genVarName :: Gen String
genVarName = Gen.element getVarList

genExprState :: Int -> Gen (ExprState Double)
genExprState n = do
    variables <- Gen.subsequence getVarList
    values <- Gen.list (Range.singleton $ length variables) (genDouble n)
    return $ zip variables values

genBinOp :: Gen BinOperator
genBinOp = Gen.element [Addition, Subtraction, Multiplication, Division, Exponentiation]

genExpr :: Int -> Gen (Expr Double)
genExpr n =
  Gen.recursive
    Gen.choice
    [
      numGen,
      varGen
    ]
    [
      binOpGen,
      unOpGen
    ]
  where
    numGen = Number <$> genDouble n
    varGen = Var <$> genVarName
    binOpGen = do
      op <- genBinOp
      Gen.subterm2 (genExpr n) (genExpr n) (Operation op)
    unOpGen = do
      Gen.subterm (genExpr n) SquareRoot

genExprInt :: Int -> Gen (Expr Int)
genExprInt n =
  Gen.recursive
    Gen.choice
    [
      numGen,
      varGen
    ]
    [
      binOpGen,
      unOpGen
    ]
  where
    numGen = Number <$> genInt n
    varGen = Var <$> genVarName
    binOpGen = do
      op <- genBinOp
      Gen.subterm2 (genExprInt n) (genExprInt n) (Operation op)
    unOpGen = do
      Gen.subterm (genExprInt n) SquareRoot

doubleToIntExpr :: Expr Double -> Expr Int 
doubleToIntExpr (Number numb) = Number $ round numb
doubleToIntExpr (SquareRoot expr) = SquareRoot $ doubleToIntExpr expr 
doubleToIntExpr (Operation op expr expr') = Operation op (doubleToIntExpr expr) (doubleToIntExpr expr')
doubleToIntExpr (Var name) = Var name