-- |
-- Module       : Data.Semiring.Tropical.Skel
-- Description  : A module for Tropical whatever
-- Copyright    : (c) Peter Harpending, 2014
-- License      : OtherLicense
-- Maintainer   : pharpend2@gmail.com
-- Stability    : experimental
-- Portability  : Linux
module Data.Semiring.Tropical.Matrix where

import qualified Data.Matrix as Mat
import qualified Data.Vector as Vec
import Data.Semiring.Tropical.Scalar

-- |A tropical matrix is just a matrix of tropical scalars.
type Matrix = Mat.Matrix Scalar

-- |Likewise, a tropical vector is just a vector os 
type Vector = Vec.Vector Scalar
