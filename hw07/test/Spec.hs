import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure )
import Expression ( Expr(..) )
import Parser ( runParser )
import ExpressionParser ( parseNumber, parseVariable, parseSquareRoot, parseBinOperation, parseExpression )
import Data.Maybe ( Maybe(..), fromJust )
import BinOperator (BinOperator(..))
import Text.Read ( readMaybe )

testParseGroups :: TestTree
testParseGroups = testGroup "TestParse" [ testParseNumber, testParseVariable, testParseSqrt, testParseOperation, testParseExpression ]
    where 
        testParseNumber = testGroup "Number"
            [
                testCase "5" $ runParser parseNumber "5" @?= Just ("", Number 5),
                testCase "05" $ runParser parseNumber "05" @?= Just ("", Number 5),
                testCase "-5" $ runParser parseNumber "-5" @?= Nothing,
                testCase "05a" $ runParser parseNumber "05a" @?= Just ("a", Number 5),
                testCase "*" $ runParser parseNumber "*" @?= Nothing
            ]
        testParseVariable = testGroup "Variable"
            [
                testCase "xyz5" $ runParser parseVariable "xyz5" @?= Just ("", Var "xyz5"),
                testCase "xyz5*" $ runParser parseVariable "xyz5*" @?= Just ("*", Var "xyz5"),
                testCase "1x" $ runParser parseVariable "1x*" @?= Nothing,
                testCase "*" $ runParser parseVariable "*" @?= Nothing,
                testCase "sqrt" $ runParser parseVariable "sqrt" @?= Nothing,
                testCase "if" $ runParser parseVariable "if" @?= Nothing
            ]
        testParseSqrt = testGroup "Sqrt"
            [
                testCase "sqrt 123" $ runParser parseSquareRoot "sqrt 123" @?= Just ("", SquareRoot $ Number 123),
                testCase "sqrt" $ runParser parseSquareRoot "sqrt" @?= Nothing,
                testCase "sqr" $ runParser parseSquareRoot "sqrt" @?= Nothing,
                testCase "sqrt xyz" $ runParser parseSquareRoot "sqrt xyz" @?= Just ("", SquareRoot $ Var "xyz"),
                testCase "sqrt xyz." $ runParser parseSquareRoot "sqrt xyz." @?= Just (".", SquareRoot $ Var "xyz")
            ]
        testParseOperation = testGroup "BinOperation"
            [
                testCase "+ 123 45" $ runParser parseBinOperation "+ 123 45" @?= Just ("", Operation Addition (Number 123) (Number 45)),
                testCase "- 123 45" $ runParser parseBinOperation "- 123 45" @?= Just ("", Operation Subtraction (Number 123) (Number 45)),
                testCase "* 123 45" $ runParser parseBinOperation "* 123 45" @?= Just ("", Operation Multiplication (Number 123) (Number 45)),
                testCase "/ 123 45" $ runParser parseBinOperation "/ 123 45" @?= Just ("", Operation Division (Number 123) (Number 45)),
                testCase "^ 123 45" $ runParser parseBinOperation "^ 123 45" @?= Just ("", Operation Exponentiation (Number 123) (Number 45)),
                testCase "% 123 45" $ runParser parseBinOperation "% 123 45" @?= Nothing,
                testCase "+ 123" $ runParser parseBinOperation "+ 123" @?= Nothing
            ]
        testParseExpression = testGroup "Expression"
            [
                testCase "123" $ runParser parseExpression "123" @?= Just ("", Number 123),
                testCase "xyz" $ runParser parseExpression "xyz" @?= Just ("", Var "xyz"),
                testCase "sqrt 123" $ runParser parseExpression "sqrt 123" @?= Just ("", SquareRoot $ Number 123),
                testCase "sqrt xyz" $ runParser parseExpression "sqrt xyz" @?= Just ("", SquareRoot $ Var "xyz"),
                testCase "+ 123 45" $ runParser parseExpression "+ 123 45" @?= Just ("", Operation Addition (Number 123) (Number 45)),
                testCase "* xyz 123" $ runParser parseExpression "* xyz 123" @?= Just ("", Operation Multiplication (Var "xyz") (Number 123)),
                testCase "/ xyz sqr" $ runParser parseExpression "/ xyz sqr" @?= Just ("", Operation Division (Var "xyz") (Var "sqr")),
                testCase "+ 123 * 45 6" $ runParser parseExpression "+ 123 * 45 6" @?= Just ("", Operation Addition (Number 123) (Operation Multiplication (Number 45) (Number 6))),
                testCase "+ * 123 45 6" $ runParser parseExpression "+ * 123 45 6" @?= Just ("", Operation Addition (Operation Multiplication (Number 123) (Number 45)) (Number 6)),
                testCase "/ sqrt 123 xyz" $ runParser parseExpression "/ sqrt 123 xyz" @?= Just ("", Operation Division (SquareRoot (Number 123)) (Var "xyz"))
            ]


main :: IO ()
main = defaultMain testParseGroups
