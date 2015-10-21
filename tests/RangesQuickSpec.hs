-----------------------------------------------------------------------------
--
-- Module      :  RangesQuickSpec
-- Copyright   :  (c) 2015 Nicolas Del Piano
-- License     :  MIT
--
-- Maintainer  :  Nicolas Del Piano <ndel314@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- QuickCheck tests for numeric ranges.
--
-----------------------------------------------------------------------------

module RangesQuickSpec (main, spec) where

import Numeric.Ranges
import Numeric.Ranges.Internal
import RangesSupport

import Test.Hspec
import Test.HUnit
import Test.QuickCheck

import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "Test suite" $ do
    it "example" $ do
      property $ True

{- spec :: Spec
spec = do
  describe "Test suite" $ do

    describe "Ranges" $ do
      describe "Invariant: in every range stands that lower bound <= upper bound" $ do
        it "[x, y] ==> x <= y" $
          property $ \x y -> x ~~ y ==> value x <= value y
-}

main :: IO ()
main = hspec spec
