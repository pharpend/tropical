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
testOrdering x y = (compare x y) == (compareTropical x y)

testTS :: Real a => TestTrop a
testTS x y = x .+. y == min x y

testTSCommute :: Real a => TestTrop a
testTSCommute x y = x .+. y == y .+. x

testTPCommute :: Real a => TestTrop a
testTPCommute x y = x .*. y == y .*. x

testTECommute :: Real a => TestTrop a
testTECommute x y = x .^. y == y .^. x

testTLog :: (Fractional a, Real a) => TestTrop a
testTLog b@(Tropical x) v@(Tropical y) = logT b v == (Tropical $ y / x)
testTLog _ _                           = False

main = do
    doubleChk testOrdering
    doubleChk testTS
    doubleChk testTSCommute
    doubleChk testTPCommute
    doubleChk testTECommute
    doubleChk testTLog

  where 
    doubleChk tst = do                        
      quickCheck (tst :: TestTrop Double)    
      quickCheck ((\x -> False) :: Int -> Bool)
