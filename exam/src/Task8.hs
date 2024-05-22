module Task8 ( Tree(..), heightCPS ) where

data Tree a = Leaf | Node (Tree a) a (Tree a)

heightCPS :: Tree a -> (Int -> r) -> r 
heightCPS Leaf k = k 0
heightCPS (Node l _ r) k = heightCPS l $ \lh -> heightCPS r $ \rh -> k $ 1 + max lh rh
