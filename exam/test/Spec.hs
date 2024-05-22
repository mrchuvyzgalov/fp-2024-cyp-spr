import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure, Assertion )
import Task5
import Task6
import Task8

testTask5 :: TestTree
testTask5 = testGroup "TestTask5"
    [
        testCase "show -123{4}" $ show (Numb Negative [3, 2, 1] 4) @?= "-123{4}",
        testCase "show 123{4}" $ show (Numb Positive [3, 2, 1] 4) @?= "123{4}",
        testCase "show 0{4}" $ show (Numb Zero [0] 4) @?= "0{4}",
        testCase "11{2} == 3{10}" $ Numb Positive [1, 1] 2 == Numb Positive [3] 10 @?= True,
        testCase "11{2} < 4{10}" $ Numb Positive [1, 1] 2 < Numb Positive [4] 10 @?= True,
        testCase "11{2} > 4{10}" $ Numb Positive [1, 1] 2 > Numb Positive [4] 10 @?= False
    ]

testTask6 :: TestTree
testTask6 = testGroup "TestTask6"
    [
        testCase "rotate 2 'abcdef' == 'cdefab'" $ Task6.rotate 2 "abcdef" @?= "cdefab",
        testCase "rotate (-2) 'abcdef' == 'efabcd'" $ Task6.rotate (-2) "abcdef" @?= "efabcd"
    ]

testTask8 :: TestTree
testTask8 = testGroup "TestTask8"
    [
        testCase "heightCPS Leaf id = 0" $ heightCPS Leaf id @?= 0,
        testCase "heightCPS (Node (Node Leaf 1 Leaf) 1 Leaf) id = 2" $ heightCPS (Node (Node Leaf 1 Leaf) 1 Leaf) id @?= 2
    ]


testExam :: TestTree
testExam = testGroup "TeskExam" [ testTask5, testTask6, testTask8 ]

main :: IO ()
main = defaultMain testExam
