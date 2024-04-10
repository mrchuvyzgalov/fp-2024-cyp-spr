import Test.Tasty

import qualified Test.Sort
import qualified Test.List
import qualified Test.Expr
import qualified Test.Unit
import qualified Test.Simplify
import qualified Test.Parser

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Sort" Test.Sort.props
                , testGroup "List" Test.List.props
                , testGroup "Expr" Test.Expr.props
                , testGroup "Unit" Test.Unit.unitTests
                , testGroup "Simplify" Test.Simplify.props
                , testGroup "Parser" Test.Parser.props
                ])