module Main (main) where

import ExpressionParser ( parseExpression )
import Parser ( runParser, parseInt )
import Data.Maybe ( fromJust )
import Data.Either ( rights )
import Eval ( runEval, ExprState )
import Text.Read ( readMaybe )

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
        putStrLn "Enter state as [(name, value)]"
        stateStr <- getLine
        case readMaybe stateStr of
          Nothing -> do
            putStrLn "State was not parsed"
            readEnv
          Just state -> return state
    evalParsedExpr expr = do
        env <- readEnv
        let evaledResult = runEval expr env
            in case evaledResult of
                Right x -> print $ show x
                Left error -> print $ show error
        return ()
