module Main (main) where

import ExpressionParser ( parseExpression )
import Parser ( runParser, parseInt )
import Data.Maybe ( fromJust )
import Data.Either ( rights )
import Eval ( runEval, ExprState )

main :: IO ()
main = do
  putStrLn "Enter expression in prefix notation:"
  exprStr <- getLine
  case runParser parseExpression exprStr of
    Just ("", expr) -> evalParsedExpr expr
    _ -> print "Expression was not parsed"
  return ()
  where
    readEnv :: IO (ExprState Double)
    readEnv = do
        putStrLn "Enter names of variables like this 'a b c d'"
        varNames <- getLine
        putStrLn "Enter values of variables like this '1 2 3 4'"
        varValues <- getLine
        let names = words varNames
            values = map fromIntegral (rights $ map parseInt (words varValues))
            in return $ zip names values 

    evalParsedExpr expr = do
        env <- readEnv
        let evaledResult = runEval expr env
            in case evaledResult of
                Right x -> print $ show x
                Left error -> print $ show error
        return ()
