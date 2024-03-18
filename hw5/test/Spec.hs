import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure )

import Simplify (simplify)
import Eval (eval)
import BinOperator ( BinOperator(..) )
import Expression ( Expr(..) )
import Error ( Error(..) )

import qualified Data.Map.Strict as M 

testEvalGroups :: TestTree
testEvalGroups = testGroup "TestEval" [ testEvalGroup ]
    where 
        testEvalGroup = testGroup "Eval"
            [
                testCase "5 = 5" $ eval (Number 5) M.empty @?= Right 5,
                testCase "x = 5" $ eval (Var "x") (M.singleton "x" 5) @?= Right 5,
                testCase "x = error" $ eval (Var "x") M.empty @?= Left (UnknownVariableError "x"),
                testCase "sqrt(4) = 2" $ eval (SquareRoot (Number 4)) M.empty @?= Right 2,
                testCase "sqrt(-2) = error" $ eval (SquareRoot (Number (-2))) M.empty @?= Left SquareRootError,
                testCase "1 + 2 = 3" $ eval (Operation Addition (Number 1) (Number 2)) M.empty @?= Right 3,
                testCase "3 - 2 = 1" $ eval (Operation Subtraction (Number 3) (Number 2)) M.empty @?= Right 1,
                testCase "2 * 3 = 6" $ eval (Operation Multiplication (Number 2) (Number 3)) M.empty @?= Right 6,
                testCase "2 ^ 3 = 8" $ eval (Operation Exponentiation (Number 2) (Number 3)) M.empty @?= Right 8,
                testCase "6 / 3 = 2" $ eval (Operation Division (Number 6) (Number 3)) M.empty @?= Right 2,
                testCase "6 / 0 = error" $ eval (Operation Division (Number 6) (Number 0)) M.empty @?= Left DividingByZeroError
            ]


testSimplifyGroups :: TestTree
testSimplifyGroups = testGroup "TestSimplify" [ testSimplifyGroup ]
    where
        testSimplifyGroup = testGroup "Simplify"
            [
                testCase "5 + 0 = 5" $ simplify (Operation Addition (Number 5) (Number 0)) @?= Number 5,
                testCase "0 + 5 = 5" $ simplify (Operation Addition (Number 0) (Number 5)) @?= Number 5,
                testCase "5 - 0 = 5" $ simplify (Operation Subtraction (Number 5) (Number 0)) @?= Number 5,
                testCase "5 * 0 = 0" $ simplify (Operation Multiplication (Number 5) (Number 0)) @?= Number 0,
                testCase "0 * 5 = 0" $ simplify (Operation Multiplication (Number 0) (Number 5)) @?= Number 0,
                testCase "5 * 1 = 5" $ simplify (Operation Multiplication (Number 5) (Number 1)) @?= Number 5,
                testCase "1 * 5 = 5" $ simplify (Operation Multiplication (Number 1) (Number 5)) @?= Number 5,   
                testCase "5 / 1 = 5" $ simplify (Operation Division (Number 5) (Number 1)) @?= Number 5,    
                testCase "sqrt(0) = 0" $ simplify (SquareRoot (Number 0)) @?= Number 0,  
                testCase "sqrt(1) = 1" $ simplify (SquareRoot (Number 1)) @?= Number 1 
            ]



main :: IO ()
main = defaultMain $ testGroup "Expressions" [ testEvalGroups, testSimplifyGroups ]
