module Reals (R(Plus, Minus, Fig, Point, End)) where

data Z = Base Integer Integer
data R = Plus R | Minus R | Fig Z R | Point R | End

instance Show Z where
    show (Base x y) | y >= x = show (div y x) ++ show (mod y x)

    show (Base 16 15) = "f"
    show (Base 16 14) = "e"
    show (Base 16 13) = "d"
    show (Base 16 12) = "c"
    show (Base 16 11) = "b"
    show (Base 16 10) = "a"

    show (Base x y) | x > 10 && x /= 16 = "|" ++ show y ++ "|"

    show (Base x y) = show y

instance Show R where
    show (Plus x)   = "" ++ show x
    show (Minus x)  =  "-" ++ show x
    show (Fig x y)  = show x ++ show y
    show (Point x)  = "." ++ show x
    show End        = ""

--instance Num R where
--    (Point x) + (Point y) = 

recNewton'sMethod f x n | n <= 0 = x
recNewton'sMethod f x n = x2 - f x2 / f' x2
    where x2 = recNewton'sMethod f x (n-1)
          f' = \x -> (f (x + e) - f (x - e)) / (2 * e)
          e = 0.001

newton'sMethod' f f' x it | it <= 0 = x
newton'sMethod' f f' x it           = newton'sMethod' f f' x2 (it-1)
    where                       x2 = x - f x / f' x

newton'sMethod :: (Ord t, Fractional t) => (t -> t) -> (t -> t) -> t -> t -> t
newton'sMethod f f' prc x
    | abs (x2 - x) < prc = x2
    | otherwise          = newton'sMethod f f' x2 prc
    where             x2 = x - f x / f' x

twelve = Plus (Fig (Base 10 1) (Fig (Base 10 2) End))
twoAndaHalf = Plus (Fig (Base 10 2) (Point (Fig (Base 10 5) End)))
negativePointTwo = Minus (Point (Fig (Base 10 2) End))