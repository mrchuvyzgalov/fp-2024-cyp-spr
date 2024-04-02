module ExpressionParser where

import Parser ( Parser(..), satisfy )
import Expression ( Expr(..) )
import Control.Applicative ( Alternative((<|>)) )
import Data.Char ( isAlpha, isAlphaNum, isDigit, digitToInt )
import Data.Maybe ( Maybe(..), isJust )
import Data.List ( elem )
import Safe ( headMay )
import BinOperator ( BinOperator(..) )

parseNumber :: Parser (Expr Double)
parseNumber = do
  h <- satisfy isDigit
  t <- go
  return $ Number $ fromIntegral $ toDigit (h : t)
  where
    toDigit = foldl1 (\a x -> a * 10 + x) . map digitToInt
    go = (do
        x <- satisfy isDigit
        y <- go
        return (x : y))
      <|>
        return []

parseIdentifier :: Parser String
parseIdentifier = do
    h <- satisfy isAlpha
    t <- go
    return (h : t)
    where
        go = (do
            x <- satisfy isAlphaNum
            y <- go
            return (x : y))
          <|>
            return []


parseVariable :: Parser (Expr Double)
parseVariable = do
    variable <- parseIdentifier
    if variable `elem` keywords
            then Parser $ const Nothing
            else return $ Var variable
    where
        keywords = ["if", "then", "else", "sqrt"]

parseWord :: String -> Parser String
parseWord word = do
    identifier <- parseIdentifier
    case identifier of
        word -> return identifier
        _ -> Parser $ const Nothing

parseSquareRoot :: Parser (Expr Double)
parseSquareRoot = do
    parseWord operator
    satisfy (== ' ')
    SquareRoot <$> parseExpression
    where
        operator = "sqrt"

parseBinOperation :: Parser (Expr Double)
parseBinOperation = do
    binOp <- satisfy (`elem` "+-*^/")
    satisfy (== ' ')
    expr <- parseExpression
    satisfy (== ' ')
    expr' <- parseExpression
    case binOp of
        '+' -> return (Operation Addition expr expr')
        '-' -> return (Operation Subtraction expr expr')
        '*' -> return (Operation Multiplication expr expr')
        '/' -> return (Operation Division expr expr')
        '^' -> return (Operation Exponentiation expr expr')


parseExpression :: Parser (Expr Double)
parseExpression = parseNumber <|> parseVariable <|> parseSquareRoot <|> parseBinOperation