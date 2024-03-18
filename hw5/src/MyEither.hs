module MyEither where

data MyEither a b 
    = MyLeft a
    | MyRight b
    deriving (Eq)

instance Functor (MyEither l) where
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