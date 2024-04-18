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

genExpr :: (Int -> Gen a) -> Int -> Gen (Expr a)
genExpr genNumb n =
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
    numGen = Number <$> genNumb n
    varGen = Var <$> genVarName
    binOpGen = do
      op <- genBinOp
      Gen.subterm2 (genExpr genNumb n) (genExpr genNumb n) (Operation op)
    unOpGen = do
      Gen.subterm (genExpr genNumb n) SquareRoot