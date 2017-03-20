module Naturals where

data N = Zero | Succ N

basis = ['0'..'9']

--basis = ['0'..'9'] ++ ['a'..'f']

instance Num N where

    Zero + Zero = Zero
    n + Zero = n
    Zero + n = n
    Succ n + m = n + Succ m

    Zero * _ = Zero
    _ * Zero = Zero
    n * Succ m = n + n * m

    Zero - _ = Zero
    n - Zero = n
    Succ n - Succ m = n - m

    abs x = x
    signum x = Succ Zero
    fromInteger n | n <= 0 = Zero
                  | otherwise = Succ (fromInteger (n-1))

instance Eq N where
    Zero == Zero = True
    _ == Zero = False
    Zero == _ = False
    Succ n == Succ m = n == m

instance Ord N where
    Zero <= Zero = True
    Zero <= _ = True
    _ <= Zero = False
    Succ n <= Succ m = n <= m

base = fromIntegral . length $ basis

instance Show N where

    show Zero = [head basis]

    show n | n < base = [extract basis n]
        where extract l Zero = head l
              extract l (Succ n) = extract (tail l) n

    show n = show (div' n base) ++ show (mod' n base)
    
        where   div' n m | n < m = Zero
                div' n m | n == m = 1::N
                div' n m = 1 + div' (n-m) m

                mod' n m | n < m = n
                mod' n m = mod' (n-m) m
