module Reals (R(End, Point, Real)) where

type Z = Integer
data R = End | Point R | Real Z R | Plus Z R | Minus Z R

instance Show R where
    show End        = ""
    show (Point x)  = "." ++ show x
    show (Real x y) = show x ++ show y

--instance Num R where
--    x + y = 

recNewton'sMethod f x n | n <= 0 = x
recNewton'sMethod f x n = x2 - f x2 / f' x2
    where x2 = recNewton'sMethod f x (n-1)
          f' = \x -> (f (x + e) - f (x - e)) / (2 * e)
          e = 0.001

newton'sMethod' f f' x it | it <= 0 = x
newton'sMethod' f f' x it           = newton'sMethod' f f' x2 (it-1)
    where                       x2 = x - f x / f' x

newton'sMethod f f' prc x
    | abs (x2 - x) < prc = x2
    | otherwise          = newton'sMethod f f' x2 prc
    where             x2 = x - f x / f' x