-- |
-- Module       : Data.Semiring.Tropical.Skel
-- Description  : A module for Tropical whatever
-- Copyright    : (c) Peter Harpending, 2014
-- License      : OtherLicense
-- Maintainer   : pharpend2@gmail.com
-- Stability    : experimental
-- Portability  : Linux

module Data.Semiring.Tropical where

import Data.Semiring

-- |The tropical semiring is {R union {infinity}, '.+.', '.*.'}. This
-- class says that '.+.' and '.*.' need to be defined for any tropical
-- *thing*, be it a scalar, vector, matrix, what have you. It also
-- requires that you be able to determine if two tropical things are
-- equal to each other, and that the two tropical things be sortable.
class (Ord t, Semiring t) => Tropical t
