-- Data.Semiring.Tropical.Scalar - a file for tropical numbers
--
-- Copyright (c) 2014, Peter Harpending. <pharpend2@gmail.com>
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
-- notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above
-- copyright notice, this list of conditions and the following
-- disclaimer in the documentation and/or other materials provided
-- with the distribution.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
-- INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
-- OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE RankNTypes #-}

-- |Module for Tropical Scalars
module Data.Semiring.Tropical.Scalar where

import Data.Ord
import Data.Semiring
import Data.Semiring.Tropical.Tropical

-- |Data type for a tropical scalar. You can either have a normal
-- scalar, which is just a real number. Or, you can have Infinity,
-- which behaves like zero.
data Scalar = Scalar { real :: (Real a) => a }
            | Infinity

-- |Making the scalar tropical. '.+.' is the minimum, and '.*.' is the
-- sum. Infinity is the additive identity, and the multiplicative
-- zero.
instance Semiring Scalar where
  zero  = Infinity
  one   = Scalar 1

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
