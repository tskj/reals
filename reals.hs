module Reals    ( R(..)
                , MajorReal(..)
                , MinorReal(..)
                ) where

import Ints

data R = Positive MajorReal | Negative MajorReal

infixr 5 :-:
infixr 5 :.:
infixr 1 :.
data MajorReal = Point MinorReal | (:-:) Z MajorReal
data MinorReal = (:.) | (:.:) Z MinorReal

instance Num R where

    Positive x + Positive y = Positive $ x+y -- Most significant digit might be larger than base b.
    Positive x + Negative y = Positive $ x-y -- Needs to be handled in every one of these.
    Negative x + Negative y = Negative $ x+y
    x + y = y + x

    Positive x * Positive y = Positive $ x*y
    Positive x * Negative y = Negative $ x*y
    Negative x * Negative y = Positive $ x*y
    x * y = y * x

    negate (Positive x) = Negative x
    negate (Negative x) = Positive x

    abs (Negative x) = Positive x
    abs = id

    signum (Positive _) = Positive 1
    signum (Negative _) = Negative 1

    fromInteger n | n < 0  = negate . fromInteger . negate $ n
    fromInteger n = Positive $ fromInteger (div n 10 * 10) + fromInteger (mod n 10)

instance Num MajorReal where

    (n1 :-: Point fr1) + (n2 :-: Point fr2) = n1 + n2 + n3 :-: Point fr3
        where fr3 = case fr1 + fr2 of
                        (:.) -> (:.)
                        Base b n :.: fr = Base b (mod n b) :.: fr
              n3  = case fr1 + fr2 of
                        (:.) -> 0
                        Base b n :.: _ = Base b $ div n b

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