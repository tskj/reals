module Ints where

data Z = Base Integer Integer

instance Show Z where
    show (Base x y) | y >= x = "|" ++ show (Base x (div y x)) ++ show (Base x (mod y x)) ++ "|"

    show (Base 16 15) = "f"
    show (Base 16 14) = "e"
    show (Base 16 13) = "d"
    show (Base 16 12) = "c"
    show (Base 16 11) = "b"
    show (Base 16 10) = "a"

    show (Base x y) = show y

instance Num Z where

    Base b x + Base c y = Base d (x + y)
        where d = max b c
    Base b x * Base c y = Base d (x * y)
        where d = max b c

    negate (Base b x) = Base b (negate x)
    abs (Base b x) = Base b (abs x)
    signum (Base b x) = Base b (signum x)
    fromInteger n = Base 10 n

instance Eq Z where
    Base _ x == Base _ y = x == y

instance Ord Z where
    Base _ x <= Base _ y = x <= y

