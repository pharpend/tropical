{-# LANGUAGE RankNTypes #-}

-- |
-- Module       : Data.Semiring.Tropical.Scalar
-- Description  : A module for Tropical scalars
-- Copyright    : (c) Peter Harpending, 2014
-- License      : BSD3
-- Maintainer   : pharpend2@gmail.com
-- Stability    : experimental
-- Portability  : Linux

module Data.Semiring.Tropical.Scalar where

import Data.Ord
import Data.Semiring
import Data.Semiring.Tropical

-- |Data type for a tropical scalar. You can either have a normal
-- scalar, which is just a real number. Or, you can have Infinity,
-- which behaves like zero.
data Scalar = Scalar { real :: (Real a) => a }
            | Infinity

-- |Tropical things alone don't have any of their own functions.
instance Tropical Scalar

-- |Making the tropical scalar a semiring. '.+.' is the minimum, and
-- '.*.' is the sum. Infinity is the additive identity, and the
-- multiplicative zero.
instance Semiring Scalar where
  zero  = Infinity
  one   = Scalar 0

  a .+. Infinity  = a
  Infinity .+. b  = b
  a .+. b         = min a b

  a .*. b
    | Infinity==a || Infinity==b  = Infinity
    | otherwise                   = Scalar $ (real a) + (real b)

-- |Defining how to test for equality with scalars. A normal scalar is
-- obviously not equal to infinity. Furthermore, infinity is obviously
-- equal to infinity. To compare two scalars, you simply compare their
-- real value.
instance Eq Scalar where
  Scalar _ == Infinity    = False
  Infinity == Scalar _    = False
  Infinity == Infinity    = True
  Scalar a == Scalar b    = a==b

  Scalar _ /= Infinity  = True
  Infinity /= Scalar _  = True
  Infinity /= Infinity  = False
  Scalar a /= Scalar b  = a/=b

-- |To order scalars, we just compare their real values. Unless one of
-- them is infinity, of course.
instance Ord Scalar where
  compare Infinity Infinity     = EQ
  compare (Scalar _) Infinity   = LT
  compare Infinity (Scalar _)   = GT
  compare a b                   = comparing real a b

-- |Tropical exponentiation - same as classical multiplication. A
-- mildly interesting correlary is that tropical exponentiation is
-- commutative. That is, y .^. x = x .^ y, for x and y tropical.
(.^.) :: Scalar -> Scalar -> Scalar
a .^. b
  | Infinity==a || Infinity==b  = Infinity
  | otherwise                   = Scalar $ (real a) * (real b)

-- |Tropical division. Remember, if Infinity is tropical zero, then
-- you can't divide by it!
(./.) :: Scalar -> Scalar -> Scalar
_ ./. Infinity          = undefined
Infinity ./. b          = b
a ./. b                 = Scalar $ (real a) - (real b)
