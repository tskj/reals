module Reals (R(Positive, Negative), MajorReal(Point, MajorReal), MinorReal(End, MinorReal)) where

data Z = Base Integer Integer
data R = Positive MajorReal | Negative MajorReal

data MajorReal = Point MinorReal | MajorReal Z MajorReal
data MinorReal = End | MinorReal Z MinorReal

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
    show (Positive x) = show x
    show (Negative x) = "-" ++ show x

instance Show MajorReal where
    show (MajorReal x y) = show x ++ show y
    show (Point End) = ""
    show (Point x) = "." ++ show x

instance Show MinorReal where
    show End = ""
    show (MinorReal x y) = show x ++ show y

--instance Num R where
--    (Point x) + (Point y) = Point (x + y)

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

twelve = Positive (MajorReal (Base 10 1) (MajorReal (Base 10 2) (Point End)))
twoAndaHalf = Positive (MajorReal (Base 10 2) (Point (MinorReal (Base 10 5) End)))
negativePointTwo = Negative (Point (MinorReal (Base 10 2) End))