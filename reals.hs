module Reals (R(Positive, Negative), MajorReal(Point, (:-:)), MinorReal((:.), (:.:))) where

data Z = Base Integer Integer
data R = Positive MajorReal | Negative MajorReal

infixr 5 :-:
infixr 5 :.:
infixr 1 :.

data MajorReal = Point MinorReal | (:-:) Z MajorReal
data MinorReal = (:.) | (:.:) Z MinorReal

instance Show Z where
    show (Base x y) | y >= x = "|" ++ show (Base x (div y x)) ++ show (Base x (mod y x)) ++ "|"

    show (Base 16 15) = "f"
    show (Base 16 14) = "e"
    show (Base 16 13) = "d"
    show (Base 16 12) = "c"
    show (Base 16 11) = "b"
    show (Base 16 10) = "a"

    show (Base x y) = show y

instance Show R where
    show (Positive x) = show x
    show (Negative x) = "-" ++ show x

instance Show MajorReal where
    show (x :-: y)      = show x ++ show y
    show (Point (:.))   = ""
    show (Point x)      = "." ++ show x

instance Show MinorReal where
    show (:.) = ""
    show (x :.: y) = show x ++ show y

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

twelve = Positive $ 1 :-: 2 :-: Point (:.)
twoAndaHalf = Positive $ 2 :-: Point (5 :.: (:.))
negativePointTwo = Negative $ Point (2 :.: (:.))

unitTest = do 
    print twelve
    print twoAndaHalf
    print negativePointTwo