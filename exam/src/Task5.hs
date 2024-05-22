module Task5 where

-- digits = значение в системе счисления base
-- base - система счисления

data Sign = Zero | Positive | Negative
data Numb = Numb { sign :: Sign, digits :: [Int], base :: Int }

instance Show Sign where
    show Negative = "-"
    show _ = ""

signToInt :: Sign -> Int
signToInt Zero = 0
signToInt Positive = 1
signToInt Negative = -1

toInt :: Numb -> Int
toInt Numb { sign = s, digits = ds, base = b } = let
    abs = fst $ foldl (\(acc, st) x -> (acc + x * st, st * b)) (0, 1) ds
    in signToInt s * abs


instance Show Numb where
    show Numb { sign = s, digits = ds, base = b } = show s ++ concatMap show (reverse ds) ++ "{" ++ show b ++ "}"

instance Eq Numb where
    (==) x x' = toInt x == toInt x'

instance Ord Numb where
    compare x x' = compare (toInt x) (toInt x')

