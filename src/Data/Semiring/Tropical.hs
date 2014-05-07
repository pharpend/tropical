{-|
Module       : Data.Semiring.Tropical
Description  : The definition of tropicality
Copyright    : (c) Peter Harpending, 2014
License      : BSD3
Maintainer   : pharpend2@gmail.com
Stability    : experimental
Portability  : Linux

This file just contains the minimal definition of "tropical," meaning
any tropical object has to be an ordered semiring.

-}

module Data.Semiring.Tropical where

import Data.Semiring

-- |The tropical semiring is {R union {infinity}, '.+.', '.*.'}. This
-- class says that '.+.' and '.*.' need to be defined for any tropical
-- *thing*, be it a scalar, vector, matrix, what have you. It also
-- requires that you be able to determine if two tropical things are
-- equal to each other, and that the two tropical things be sortable.
class (Ord t, Semiring t) => Tropical t
