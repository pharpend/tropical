{-|
Module       : Data.Semiring.Tropical
Description  : The definition of tropicality
Copyright    : 2014, Peter Harpending.
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

This is a module for Tropical numbers. If you don't know what those are, read
<http://en.wikipedia.org/wiki/Tropical_geometry this Wikipedia entry>.

Tropical numbers form a 'Semiring'. Semirings are like
<https://en.wikipedia.org/wiki/Ring_(mathematics) normal rings>, but
you can't subtract.

The Tropical semiring, or 𝕋, is {ℝ ∪ {∞}, ⊕, ⊙}. Those are, in Haskell
terms, @Real@, @Infinity@, @(.+.)@, and @(.*.)@, respectively.

Tropical addition and multiplication are

a ⊕ b = min {a, b}, ∀ a, b ∈ 𝕋

a ⊙ b = a + b, ∀ a, b ∈ 𝕋

-}

module Data.Semiring.Tropical
  (
  -- * Tropical things
  -- 
  -- 
    Tropical(..)

  -- ** Tropical operations
  -- *** Artifacts of 'Semiring'
  -- $semiring-add
  , (.+.)
  , zero
  -- $semiring-mult
  , (.*.)
  , one
  -- *** Tropical-exclusive operations
  , (./.)
  , (.^.)
  , logT
  -- $ln

  -- ** Helper Things
  , Operator
  , TropicalOperator
  )
where

import Data.Semiring

-- |Tropical numbers are like real numbers, except zero is the same
-- thing as Infinity, and you can't subtract.
data Real t => Tropical t = Tropical { realValue :: t } -- ^Any tropical number
                          | Infinity                    -- ^Infinity
  deriving (Eq, Ord, Show)

-- |Helper type for binary operators  
type Operator a = a -> a -> a
-- |An operator over something tropical
type TropicalOperator a = Operator (Tropical a) 

-- $semiring-add
-- Tropical addition is the same as taking the minimum. Because
-- 
-- min {a, ∞} = a, ∀ a ∈ ℝ
-- 
-- 'Infinity' is the additive identity, or 'zero', in Semiring terms. 

-- $semiring-mult
-- 
-- Tropical multiplication is the sum. Because 
-- 
-- a + 0 = 0, ∀ a ∈ ℝ
-- 
-- @Tropical 0@ is the multiplicative identity, or 'one' in Semiring
-- terms.
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
-- you can't divide by it! So,
-- 
-- > _ ./. Infinity          = undefined
(./.) :: Real a => TropicalOperator a
_ ./. Infinity          = undefined
Infinity ./. _          = Infinity
a ./. b                 = Tropical $ (realValue a) - (realValue b)

-- |Tropical exponentiation - same as classical multiplication. A
-- mildly interesting correlary is that tropical exponentiation is
-- commutative. That is, @y .^. x = x .^. y@, ∀ x, y ∈ 𝕋
(.^.) :: Real a => TropicalOperator a
a .^. b
  | Infinity==a || Infinity==b  = Infinity
  | otherwise                   = Tropical $ (realValue a) * (realValue b)

-- |Tropical logarithm - which is roughly equal to classical
-- division. That is, @logT a b = b / a@ . Note that, in the context
-- of ℝ, you can't divide by zero, and there's no well-defined way of
-- dividing by infinity, so
--
-- > logT (Tropical 0) _ = undefined
-- > logT Infinity _     = undefined
-- 
-- However, it is well defined to have infinity or zero in the
-- divisor, so
-- 
-- > logT _ Infinity     = Infinity
-- > logT _ (Tropical 0) = Tropical 0
-- 
-- If you aren't dealing with infinity or zero, this is the normal definition
-- 
-- > logT (Tropical base) (Tropical value) = Tropical $ value / base
-- 
logT :: (Fractional a, Real a) => TropicalOperator a
logT (Tropical 0) _ = undefined
logT Infinity _     = undefined
logT _ Infinity     = Infinity
logT _ (Tropical 0) = Tropical 0
logT (Tropical base) (Tropical value) = Tropical $ value / base


-- $ln
-- 
-- Normally, when you think of logarithms, you think of e, the
-- standard log base. e is interesting for many reasons, but chiefly
-- because D(e^x) = e^x.  Deriviatives rely on differentials, which
-- are traditionally defined with subtraction.
-- 
-- Because there is no subtraction with semirings, the concept of a
-- deriviative doesn't exist, and therefore the concept of e doesn't
-- exist. So, there isn't any "natural tropical logarithm."
