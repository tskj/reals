module Reals (R(End, Point, Real)) where

type Z = Integer
data R = Plus R | Minus R | Real Z R | Point R | End

instance Show R where
    show End        = ""
    show (Point x)  = "." ++ show x
    show (Real x y) = show x ++ show y
    show (Plus x)   = "" ++ show x
    show (Minus x)  =  "-" ++ show x

--instance Num R where
--   x + y = 

recNewton'sMethod f x n | n <= 0 = x
recNewton'sMethod f x n = x2 - f x2 / f' x2
    where x2 = recNewton'sMethod f x (n-1)
          f' = \x -> (f (x + e) - f (x - e)) / (2 * e)
          e = 0.001

newton'sMethod' f f' x it | it <= 0 = x
newton'sMethod' f f' x it           = newton'sMethod' f f' x2 (it-1)
    where                       x2 = x - f x / f' x

newton'sMethod :: (Ord t, Fractional t) => (t -> t) (t -> t) -> t -> t -> t
newton'sMethod f f' prc x
    | abs (x2 - x) < prc = x2
    | otherwise          = newton'sMethod f f' x2 prc
    where             x2 = x - f x / f' x

twelve = Plus (Real 1 (Real 2 End))
twoAndaHalf = Plus (Real 2 (Point (Real 5 End)))
negativePointTwo = Minus (Point (Real 2 End))