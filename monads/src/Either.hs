module Either where 

data MyEither a b 
  = MyLeft a 
  | MyRight b 
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (MyEither a) where
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

-- Implement the instance and prove the laws
instance Applicative (MyEither a) where
  pure = MyRight
  MyLeft f <*> _ = MyLeft f
  MyRight f <*> MyRight x = MyRight (f x)
  MyRight _ <*> MyLeft x = MyLeft x

-- Proof:
-- 1. Identity
--    a) pure id <*> MyRight x = MyRight id <*> MyRight x =  MyRight (id x) = MyRight x - PROVED
--    b) pure id <*> MyLeft x = MyRight id <*> MyLeft x = MyLeft x - PROVED
--
-- 2. Composition    
--    a) pure (.) <*> u <*> v <*> w = MyRight (.) <*> MyRight x <*> MyRight y <*> w = 
--       = MyRight (. x) <*> MyRight y <*> w = MyRight (x . y) <*> w = 
--
--       a.1) = MyRight (x . y) <*> MyRight z = MyRight ((x . y) z)
--       a.2) = MyRight (x . y) <*> MyLeft z = MyLeft z
--
--    b) u <*> (v <*> w) = MyRight x <*> (MyRight y <*> w) =
--       
--       b.1) = MyRight x <*> (MyRight y <*> MyRight z) = MyRight x <*> MyRight (y z) = MyRight ((x .y) z)
--       b.2) = MyRight x <*> (MyRight y <*> MyLeft z) = MyRight x <*> MyLeft z = MyLeft z
--
--    if u or v is MyLeft => Result is MyLeft
--
-- 3. Homomorphism
--    a) pure f <*> pure x = MyRight f <*> MyRight x = MyRight (f x)
--    b) pure (f x) = MyRight (f x)
-- 
-- 4. Interchange
--    a) u <*> pure y = u <*> MyRight y =
-- 
--       a.1) = MyRight x <*> MyRight y = MyRight (x y)
--       a.2) = MyLeft x <*> MyRight y = MyLeft x
-- 
--    b) pure ($ y) <*> u = MyRight ($ y) <*> u = 
--
--       b.1) = MyRight ($ y) <*> MyRight x = MyRight (x y)
--       b.2) = MyRight ($ y) <*> MyLeft x = MyLeft x 

-- Implement the instance and prove the laws
instance Monad (MyEither a) where
  return = pure
  (>>=) (MyRight x) f = f x
  (>>=) (MyLeft x) _ = MyLeft x

-- Proof:
-- 1. Identity
--    a) return a >>= k = MyRight a >>= k = k a - PROVED 
--    b) m >>= return = 
--       
--      b.1) = MyRight x >>= return = return x = MyRigth x
--      b.2) = MyLeft x >>= return = MyLeft x
--
-- 2. Assiciativity    
--    a) m >>= (\x -> k x >>= h) =
--
--      a.1) = MyRight z >>= (\x -> k x >>= h) = (k z) >>= h = h (k z)
--      a.2) = MyLeft z >>= (\x -> k x >>= h) = MyLeft z
--    
--    b) (m >>= k) >>= h
--     
--      b.1) = (MyRight z >>= k) >>= h = (k z) >>= h = h (k z)
--      b.2) = (MyLeft z >>= k) >>= h = MyLeft z >>= h = MyLeft z