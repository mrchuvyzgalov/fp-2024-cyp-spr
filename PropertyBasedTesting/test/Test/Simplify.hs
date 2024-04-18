module Test.Simplify where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Test.Util
import Expression.Simplify
import Expression.Eval

prop_simplifiedExprResultIsEqualToExprResult :: Property
prop_simplifiedExprResultIsEqualToExprResult = property $ do
  expr <- forAll $ genExpr genDouble maxValue
  exprState <- forAll $ genExprState maxValue
  assert ((runEval expr exprState) == (runEval (simplify expr) exprState))
  where
    maxValue = 100

props :: [TestTree]
props = [testProperty "Simplified expression and expression have the same result" prop_simplifiedExprResultIsEqualToExprResult]