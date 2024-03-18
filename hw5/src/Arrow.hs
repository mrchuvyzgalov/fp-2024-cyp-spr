module Arrow where

newtype Arrow a b = Arrow ((->) a b)

instance Functor (Arrow a) where
    fmap f (Arrow g) = Arrow (f . g)

-- Proof
-- 1. fmap id == id
--    a) fmap id (Arrow g) = Arrow (id . g)
--    b) id (Arrow g) = Arrow (id . g)
--    c) (a) == (b) - PROVED
--
-- 2. fmap (f . g) == (fmap f) . (fmap g) 
--    a) fmap (f . g) (Arrow h) = Arrow ((f . g) . h) = Arrow (f . g . h)
--    b) ((fmap f) . (fmap g)) (Arrow h) = fmap f (Arrow (g . h)) = Arrow (f . g . h)
--    c) (a) == (b) - PROVED
--
-- 3. 1 - PROVED, 2 - PROVED |-> Functor Arrow - PROVED