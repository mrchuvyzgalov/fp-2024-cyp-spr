module Logger where 

data Logger l a = Logger [l] a deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (Logger l) where 
    fmap f (Logger logs a) = Logger logs (f a)

-- Proof:
-- 1. Identity
--    a) fmap id (Logger logs a) = Logger logs (id a) = Logger logs a
--    b) id (Logger logs a) = Logger logs a
--       
--
-- 2. Composition  
--    a) fmap (f . g) (Logger logs a) = Logger logs ((f . g) a)
--    b) (fmap f) . (fmap g) (Logger logs a) = (fmap f) (Logger logs (g a)) = Logger logs ((f .g) a)


-- Implement the instance and prove the laws
instance Applicative (Logger l) where 
    pure = Logger []
    (Logger f_logs f) <*> (Logger logs x) = Logger (f_logs ++ logs) (f x)

-- Proof:
-- 1. Identity
--    pure id <*> Logger logs x = Logger [] id <*> Logger logs x = Logger logs (id x) = Logger logs x
--
-- 2. Composition    
--    a) pure (.) <*> u <*> v <*> w = Logger [] (.) <*> Logger x_logs x <*> Logger y_logs y <*> Logger z_logs z = 
--       = Logger x_logs (. x) <*> Logger y_logs y <*> Logger z_logs z = Logger (x_logs ++ y_logs) (x . y) <*> Logger z_logs z
--       = Logger (x_logs ++ y_logs ++ z_logs) ((x . y) z)
--
--    b) u <*> (v <*> w) = Logger x_logs x <*> ((Logger y_logs y) <*> Logger z_logs z) =
--       = Logger x_logs x <*> (Logger (y_logs ++ z_logs) (y z) = Logger (x_logs ++ y_logs ++ z_logs) ((x . y) z) 
--
-- 3. Homomorphism
--    a) pure f <*> pure x = Logger [] f <*> Logger [] x = Logger [] (f x)
--    b) pure (f x) = Logger [] (f x)
-- 
-- 4. Interchange
--    a) u <*> pure y = Logger x_logs x <*> Logger [] y = Logger x_logs (x y)
--    b) pure ($ y) <*> u = Logger [] ($ y) <*> Logger x_logs x = Logger x_logs (x y)

-- Implement the instance and prove the laws
instance Monad (Logger l) where 
    return = pure
    (>>=) (Logger logs x) f = 
        case f x of
            Logger new_logs y -> Logger (logs ++ new_logs) y

-- Proof:
-- 1. Identity
--    a) return a >>= k = Logger [] x >>= k = k x 
--    b) m >>= return = Logger x_logs x >>= return = Logger x_logs x
--
-- 2. Assiciativity    
--    a) m >>= (\x -> k x >>= h) = Logger y_logs y >>= (\x -> k x >>= h) = Logger (y_logs ++ k_logs) (k y) >>= h
--       = Logger (y_logs ++ k_logs ++ h_logs) ((h . k) y)
--    b) (m >>= k) >>= h = (Logger y_logs y >>= k) >>= h = (Logger (y_logs ++ k_logs) (k y)) >>= h
--       = Logger (y_logs ++ k_logs ++ h_logs) ((h . k) y)
--     

-- Writes a single log message. 
-- Can be easily bound together with other logging computations.
writeLog :: l -> Logger l ()
writeLog l = Logger [l] () 

-- Logs every intermediate result 
-- ghci> factLog 5
-- Logger [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120)] 120
factLog :: Int -> Logger (Int, Int) Int 
factLog n 
  | n <= 0 = do 
      let res = 1 
      writeLog (n, res)
      return res 
  | otherwise = do 
      prev <- factLog (n - 1)
      let res = n * prev 
      writeLog (n, res)
      return res 

          