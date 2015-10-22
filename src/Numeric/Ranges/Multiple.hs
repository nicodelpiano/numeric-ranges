-----------------------------------------------------------------------------
--
-- Module      :  Numeric.Ranges.Multiple
-- Copyright   :  (c) 2015 Nicolas Del Piano
-- License     :  MIT
--
-- Maintainer  :  Nicolas Del Piano <ndel314@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- A Framework for multiple numeric ranges.
-- The main purpose of this module is to provide a simple but powerful
-- interface for working with disjoint numeric intervals.
--
-----------------------------------------------------------------------------

module Numeric.Ranges.Multiple where

import Numeric.Ranges.Internal

-- | MultipleRange
--
-- A data-type that represent multiple ranges.
-- Example: [1,3] \cup (6,10]
data MultipleRange a
  = BasicRange (Range a)
  | Union (MultipleRange a) (MultipleRange a)
  | Not (MultipleRange a)
  deriving (Show, Eq)

-- | Functor instance of MultipleRange.
instance Functor MultipleRange where
  fmap f (BasicRange r) = BasicRange $ fmap f r
  fmap f (Union ml mr) = Union (fmap f ml) (fmap f mr)
  fmap f (Not m) = Not $ fmap f m

-- | Converts a single range into a multiple range.
toMultiple :: Range a -> MultipleRange a
toMultiple = BasicRange

-- |
--
union :: Range a -> Range a -> MultipleRange a
union rl rr = Union mrl mrr
  where
  mrl = toMultiple rl
  mrr = toMultiple rr

-- |
--
complete :: MultipleRange a -> MultipleRange a
complete = undefined

-- | Check whether or not the given element is present in the multiple range.
belongs :: (Num a, Ord a) => a -> MultipleRange a -> Bool
belongs x = go
  where
  go (BasicRange r) = inX x r
  go (Union ml mr) = belongs x ml || belongs x mr
  go (Not m) = not $ belongs x m

-- |
--
cut :: MultipleRange a -> MultipleRange a -> MultipleRange a
cut = undefined 
