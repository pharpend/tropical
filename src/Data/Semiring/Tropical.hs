{-# LANGUAGE InstanceSigs #-}

{-|
Module       : Data.Semiring.Tropical
Description  : The definition of tropicality
Copyright    : 2014, Peter Harpending.
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com
Stability    : experimental
Portability  : Linux

This file just contains the minimal definition of "tropical," meaning
any tropical object has to be an ordered semiring.

-}

module Data.Semiring.Tropical where

import Data.Ord
import Data.Semiring

-- |Tropical numbers are like real numbers, except zero is the same
-- thing as Infinity, and you can't subtract.
data Real t => Tropical t = Tropical { realValue :: t } -- ^Any tropical number
                          | Infinity                    -- ^Infinity
  deriving (Eq, Ord, Show)

type Operator a = a -> a -> a

instance Real a => Semiring (Tropical a) where
  -- |Tropical addition is the same as taking the minimum
  a .+. b = min a b
  -- |Tropical multiplication is the same as the sum
  a .*. b
    | Infinity==a || Infinity==b = Infinity
    | otherwise                  = Tropical $ (realValue a) + (realValue b)

  -- |Infinity acts like zero.
  zero = Infinity
  -- |Zero acts like one.
  one = Tropical 0

-- |Tropical division. Remember, if Infinity is tropical zero, then
-- you can't divide by it!
(./.) :: Real a => Operator (Tropical a)
_ ./. Infinity          = undefined
Infinity ./. b          = Infinity
a ./. b                 = Tropical $ (realValue a) - (realValue b)

-- -- |Tropical exponentiation - same as classical multiplication. A
-- -- mildly interesting correlary is that tropical exponentiation is
-- -- commutative. That is, y .^. x = x .^. y, for x and y tropical.
(.^.) :: Real a => Operator (Tropical a)
a .^. b
  | Infinity==a || Infinity==b  = Infinity
  | otherwise                   = Tropical $ (realValue a) * (realValue b)
