module Task6 where

-- Если список бесконечен, функция никогда не завершится
rotate :: Int -> [a] -> [a]
rotate n [] = []
rotate n xs
    | n == 0 = xs
    | n > 0 = drop n xs ++ take n xs
    | otherwise = let
        n' = max 0 $ (+ n) $ length xs
        in rotate n' xs

