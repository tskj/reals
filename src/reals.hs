module Reals    ( R(..)
                , MajorReal(..)
                , MinorReal(..)
                ) where

import Naturals

data R = Positive MajorReal | Negative MajorReal

infixr 5 :-:
infixr 5 :.:
infixr 1 :.
data MajorReal = Point MinorReal | (:-:) N MajorReal
data MinorReal = (:.) | (:.:) N MinorReal

instance Num R where

    Positive x + Positive y = Positive $ normalizeM . addM $ x y
    Positive x + Negative y = Positive $ normalizeM . subM $ x y
    Negative x + Negative y = Negative $ normalizeM . addM $ x y
    x + y = y + x

    Positive x * Positive y = Positive $ normalizeM . mulM $ x y
    Positive x * Negative y = Negative $ normalizeM . mulM $ x y
    Negative x * Negative y = Positive $ normalizeM . mulM $ x y
    x * y = y * x

    negate (Positive x) = Negative x
    negate (Negative x) = Positive x

    abs (Negative x) = Positive x
    abs (Positive x) = Positive x

    signum (Positive _) = Positive 1
    signum (Negative _) = Negative 1

    fromInteger n | n < 0  = negate . fromInteger . negate $ n
    fromInteger n = Positive $ fromInteger (div n base * base) + fromInteger (mod n base)


normalizeM (x :-: y)
    | x >= base = normalizeM (div' x base :-: mod' x base :-: y)
    | otherwise = x :-: y

addM (n1 :-: Point fr1) (n2 :-: Point fr2) = n1 + n2 + n3 :-: Point fr3


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