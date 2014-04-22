-- Tropical.Types - types of tropical things
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

-- |Module for tropical things
module Tropical.Types where

class (Eq t) => Tropical t where
  (<+>) :: t -> t -> t
  (<*>) :: t -> t -> t

data Number = Number { real :: (Real a) => a }
            | Infinity

instance Tropical Number where
  (<+>) :: Number -> Number -> Number
  a <+> Infinity  = a
  Infinity <+> b  = b
  Number a <+> Number b         = Number $ min a b

  (<*>) :: Number -> Number -> Number
  a <*> b
    | Infinity==a || Infinity==b  = Infinity
    | otherwise                   = Number $ (real a) + (real b)

instance Eq Number where
  (==) :: Number -> Number ->  Bool
  Number a == Infinity    = False
  Infinity == Number b    = False
  Infinity == Infinity    = True
  Number a == Number b    = a==b

  (/=) :: Number -> Number -> Bool
  Number b /= Infinity  = True
  Infinity /= Number b  = True
  Infinity /= Infinity  = False
  Number a /= Number b  = a/=b

