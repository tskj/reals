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

    Positive x + Positive y = Positive $ x+y
    Positive x + Negative y = Positive $ x-y
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