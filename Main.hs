module Main where 

import Text.Printf (printf)
import Control.Monad (unless)
import Distribution.Simple.Utils (xargs)
import Data.Either (isLeft)

data MyEither a b 
    = MyLeft a
    | MyRight b
    deriving (Eq)

instance Functor (MyEither l) where
    fmap :: (r1 -> r2) ->  MyEither l r1 -> MyEither l r2
    fmap _ (MyLeft x) = MyLeft x
    fmap f (MyRight x) = MyRight (f x)

-- Proof:
-- 1. fmap id == id
--    
--    1.1. 
--        a) fmap id Left x = Left x
--        b) id Left x = Left x
--        c) (a) == (b) - PROVED
--    
--    1.2. 
--        a) fmap id Right x = Right (id x) = Right x
--        b) id Right x = Right x
--        c) (a) == (b) - PROVED
--
--    
-- 2. fmap (f . g) == (fmap f) . (fmap g) 
--        
--    2.1.
--        a) fmap (f . g) Left x = Left x
--        b) (fmap  f) . (fmap g) Left x = (fmap f) (fmap g Left x) = fmap f Left x = Left x
--        c) (a) == (b) - PROVED
--     
--    2.2.
--        a) fmap (f . g) Right x = Right (f . g) x 
--        b) (fmap f) . (fmap g) Right x = (fmap f) (fmap g Right x) = fmap f Right (g x) = Right (f (g x)) = Right (f . g x) 
--        c) (a) == (b) - PROVED
-- 
-- 3. 1 - PROVED, 2 - PROVED |-> Functor Either - PROVED

data Arrow a b = To a b deriving (Eq)

instance Functor (Arrow a) where
    fmap :: (b -> c) ->  Arrow a b -> Arrow a c
    fmap f (To x y) = To x (f y)

-- Proof
-- 1. fmap id == id
--    a) fmap id (To x y) = To x (id y) = To x y
--    b) id (To x y) = To x y
--    c) (a) == (b) - PROVED
--
-- 2. fmap (f . g) == (fmap f) . (fmap g) 
--    a) fmap (f . g) (To x y) = To x (f . g y)
--    b) (fmap f) . (fmap g) (To x y) = (fmap f) . (fmap g (To x y)) = fmap f (To x (g y)) = To x (f (g y)) = To x (f . g y)
--    c) (a) == (b) - PROVED
--
-- 3. 1 - PROVED, 2 - PROVED |-> Functor Arrow - PROVED

data BinOperator 
  = Addition
  | Subtraction 
  | Multiplication
  | Division
  | Exponentiation 
  deriving (Eq)

instance Show BinOperator where 
  show Addition = "+"
  show Subtraction = "-"
  show Multiplication = "*"
  show Division = "/"
  show Exponentiation = "^"

data Expr a
  = Number a
  | SquareRoot (Expr a)
  | Operation BinOperator (Expr a) (Expr a) 
  | Var String
  deriving (Eq)

instance Num a => Num (Expr a) where
    (+) x y = Operation Addition x y
    (-) x y = Operation Subtraction x y
    (*) x y = Operation Multiplication x y
    fromInteger = Number . fromInteger

instance Show a => Show (Expr a) where 
  show (Number x) = show x
  show (SquareRoot x) = "sqrt(" ++ show x ++ ")"
  show (Operation op x y) = "(" ++ show x ++ " " ++ show op ++ " " ++ show y ++ ")"
  show (Var x) = x

data Error 
  = SquareRootError
  | DividingByZeroError
  | UnknownVariableError
  deriving (Eq)

instance Show Error where 
  show SquareRootError = "You can't take the square root of a negative number"
  show DividingByZeroError = "You can't divide by zero"
  show UnknownVariableError = "A variable is unknown"

extractValues :: Either Error a -> Either Error a -> Either Error (a, a)
extractValues (Right x) (Right y) = Right (x, y)
extractValues (Left x) _ = Left x
extractValues _ (Left y) = Left y

takeOperation :: (Floating a, Ord a) => BinOperator -> a -> a -> Either Error a
takeOperation Addition x y = Right (x + y)
takeOperation Subtraction x y = Right (x - y)
takeOperation Multiplication x y = Right (x * y)
takeOperation Exponentiation x y = Right (x ** y)
takeOperation Division x y 
  | y == 0 = Left DividingByZeroError
  | otherwise = Right (x / y)

findValue :: String -> [(String, a)] -> Maybe a
findValue var [] = Nothing
findValue var (h : t) 
    | var == (getName h) = Just (getValue h)
    | otherwise = findValue var t
    where 
        getName :: (String, a) -> String
        getName (name, _) = name

        getValue :: (String, a) -> a
        getValue (_, value) = value

eval :: (Floating a, Ord a) => (Expr a) -> [(String, a)] -> Either Error a
eval (Var name) vars = 
  case findValue name vars of
    Just value -> Right value
    Nothing -> Left UnknownVariableError
eval (Number x) _ = Right x
eval (SquareRoot x) vars = 
  case eval x vars of 
    Left error -> Left error
    Right evaledX -> if evaledX < 0 then Left SquareRootError else Right $ sqrt evaledX
eval (Operation op x y) vars =
  case extractValues (eval x vars) (eval y vars) of
    Left error -> Left error
    Right (evaledX, evaledY) -> takeOperation op evaledX evaledY

simplify :: (Num a, Ord a) => Expr a -> Expr a
simplify (Operation Addition x (Number 0)) = simplify x
simplify (Operation Addition (Number 0) x) = simplify x
simplify (Operation Subtraction x (Number 0)) = simplify x
simplify (Operation Multiplication x (Number 0)) = Number 0
simplify (Operation Multiplication (Number 0) x) = Number 0
simplify (Operation Multiplication x (Number 1)) = simplify x
simplify (Operation Multiplication (Number 1) x) = simplify x
simplify (Operation Exponentiation x (Number 1)) = simplify x
simplify (Operation Division x (Number 1)) = simplify x
simplify (SquareRoot (Number 0)) = Number 0
simplify (SquareRoot (Number 1)) = Number 1
simplify x = x

cases :: [(Expr Double, Either Error Double)]
cases = [
  (Number 5, Right 5), 
  (Var "x", Right 5),
  (Var "y", Left UnknownVariableError),
  (SquareRoot (Number 4), Right 2),
  (SquareRoot (Number (-2)), Left SquareRootError),
  (Operation Addition (Number 1) (Number 2), Right 3),
  (Operation Subtraction (Number 3) (Number 2), Right 1),
  (Operation Multiplication (Number 2) (Number 3), Right 6),
  (Operation Exponentiation (Number 2) (Number 3), Right 8),
  (Operation Division (Number 6) (Number 3), Right 2),
  (Operation Division (Number 6) (Number 0), Left DividingByZeroError)
  ] 

test :: Expr Double -> Either Error Double -> IO () 
test expr expected = 
    let actual = eval expr testVars in 
    unless (expected == actual) $ describeFailure actual
  where 
    testVars = [("x", 5)]
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 

caseSimplify :: [(Expr Double, Expr Double)]
caseSimplify = [
    (Operation Addition (Number 5) (Number 0), Number 5),
    (Operation Addition (Number 0) (Number 5), Number 5),
    (Operation Subtraction (Number 5) (Number 0), Number 5),
    (Operation Multiplication (Number 5) (Number 0), Number 0),
    (Operation Multiplication (Number 0) (Number 5), Number 0),
    (Operation Multiplication (Number 5) (Number 1), Number 5),
    (Operation Multiplication (Number 1) (Number 5), Number 5),
    (Operation Division (Number 5) (Number 1), Number 5),
    (SquareRoot (Number 0), Number 0),
    (SquareRoot (Number 1), Number 1)
  ] 

testSimplify :: Expr Double -> Expr Double -> IO ()
testSimplify expr expected =
    let actual = simplify expr in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)   

main :: IO () 
main = do 
  mapM_ (uncurry test) cases 
  mapM_ (uncurry testSimplify) caseSimplify 
  