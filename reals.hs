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