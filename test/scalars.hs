-- |
-- Module       : Main
-- Description  : Tests for Tropical scalars.
-- Copyright    : 2014, Peter Harpending
-- License      : BSD3
-- Maintainer   : Peter Harpending <pharpend2@gmail.com>
-- Stability    : experimental
-- Portability  : archlinux

module Main where

import Control.Applicative
import Data.Ord
import Data.Semiring.Tropical
import Test.QuickCheck
import Test.QuickCheck.Gen

type TestTrop a = Tropical a -> Tropical a -> Bool

instance (Arbitrary a, Real a) => Arbitrary (Tropical a) where
  arbitrary = maybe Infinity Tropical <$> arbitrary

compareTropical :: Real a => Tropical a -> Tropical a -> Ordering
compareTropical Infinity Infinity = EQ
compareTropical _ Infinity        = LT
compareTropical Infinity _        = GT
compareTropical a b               = comparing realValue a b

testOrdering :: Real a => TestTrop a
testOrdering a b = (compare a b) == (compareTropical a b)

main = do
    quickCheck (testOrdering :: TestTrop Int)
    quickCheck (testOrdering :: TestTrop Double)

