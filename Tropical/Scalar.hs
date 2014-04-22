-- Tropical.Scalar - a file for tropical numbers
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

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

-- |Module for Tropical Scalars
module Tropical.Scalar where

import Data.Ord
import Tropical.Tropical

data Scalar = Scalar { real :: (Real a) => a }
            | Infinity

instance Tropical Scalar where
  (<+>) :: Scalar -> Scalar -> Scalar
  a <+> Infinity  = a
  Infinity <+> b  = b
  a <+> b         = min a b

  (<*>) :: Scalar -> Scalar -> Scalar
  a <*> b
    | Infinity==a || Infinity==b  = Infinity
    | otherwise                   = Scalar $ (real a) + (real b)

instance Eq Scalar where
  (==) :: Scalar -> Scalar ->  Bool
  Scalar a == Infinity    = False
  Infinity == Scalar b    = False
  Infinity == Infinity    = True
  Scalar a == Scalar b    = a==b

  (/=) :: Scalar -> Scalar -> Bool
  Scalar b /= Infinity  = True
  Infinity /= Scalar b  = True
  Infinity /= Infinity  = False
  Scalar a /= Scalar b  = a/=b

instance Ord Scalar where
  compare :: Scalar -> Scalar -> Ordering
  compare = comparing real

