module Test.Parser where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Util
import Data.Maybe

import Parser.Parser
import Parser.ExpressionParser
import Expression.Expression


prop_expressionIsParsedToTheSameExpression :: Property
prop_expressionIsParsedToTheSameExpression = property $ do
  expr <- forAll (genExpr genInt maxValue)
  case runParser parseExpression (showExprInPrefixNotation expr) of
    Just ("", expr') -> assert (expr == fmap round expr')
    _ -> assert False
  where
    maxValue = 100

props :: [TestTree]
props = [testProperty "Expression is parsed to the same expression" prop_expressionIsParsedToTheSameExpression]