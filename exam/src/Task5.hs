module Task5 ( Sign(..), Numb(..) ) where

-- digits = значение в системе счисления base
-- base - система счисления

data Sign = Zero | Positive | Negative deriving (Eq)
data Numb = Numb { sign :: Sign, digits :: [Integer], base :: Integer }

instance Show Sign where
    show Negative = "-"
    show _ = ""

signToInt :: Sign -> Integer
signToInt Zero = 0
signToInt Positive = 1
signToInt Negative = -1

toInt :: Numb -> Integer
toInt Numb { sign = s, digits = ds, base = b } = let
    abs = fst $ foldl (\(acc, st) x -> (acc + x * st, st * b)) (0, 1) ds
    in signToInt s * abs

toBase :: Integer -> Integer -> Numb
toBase b x = let 
    s = toSign x
    x' = abs x 
    in Numb { sign = s, digits = toDigits b x', base = b }
    where 
        toSign x
            | x < 0 = Negative
            | x == 0 = Zero
            | otherwise = Positive
        toDigits b x = reverse $ go b x []
            where
                go _ 0 acc =
                    case acc of
                        [] -> [0]
                        _ -> acc
                go b x acc = let
                    x' = x `mod` b
                    x'' = x `div` b
                    in go b x'' (x':acc)
        



instance Show Numb where
    show Numb { sign = s, digits = ds, base = b } = show s ++ concatMap show (reverse ds) ++ "{" ++ show b ++ "}"

instance Eq Numb where
    (==) x x' = toInt x == toInt x'

instance Ord Numb where
    compare x x' = compare (toInt x) (toInt x')

instance Num Numb where
    abs x@(Numb { sign = s })
        | s == Negative = x { sign = Positive }
        | otherwise = x

    signum x@(Numb { sign = s })
        | s == Negative = x { digits = [1] }
        | s == Positive = x { digits = [1] }
        | otherwise = x

    fromInteger = toBase 10

    (+) x@(Numb { base = b }) y = toBase b (toInt x + toInt y)

    (*) x@(Numb { base = b }) y = toBase b (toInt x * toInt y)

    negate x@(Numb { sign = s }) 
        | s == Negative = x { sign = Positive }
        | s == Positive = x { sign = Negative }
        | otherwise = x
