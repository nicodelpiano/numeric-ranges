-----------------------------------------------------------------------------
--
-- Module      :  RangesUnitSpec
-- Copyright   :  (c) 2015 Nicolas Del Piano
-- License     :  MIT
--
-- Maintainer  :  Nicolas Del Piano <ndel314@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Unit tests for numeric ranges.
--
-----------------------------------------------------------------------------

module RangesUnitSpec (main, spec) where

import Numeric.Ranges
import Numeric.Ranges.Internal

import Test.Hspec
import Test.HUnit

import Control.Exception (evaluate)

-- | Arbitrary defined endpoints for unit testing.
--
endpoint1 :: Endpoint Int
endpoint1 = endpoint 1 Open

-- | Arbitrary defined ranges for unit testing.
--
range1 :: Range Int
range1 = 3><10

range2 :: Range Int
range2 = 3~<10

range3 :: Range Int
range3 = 3~~10

range4 :: Range Int
range4 = 3>~10

range5 :: Range Int
range5 = (-4)><(-1)

range6 :: Range Int
range6 = (-4)>~(-1)

range7 :: Range Int
range7 = (-4)~~(-1)

range8 :: Range Int
range8 = (-4)~<(-1)

range9 :: Range Int
range9 = (-8)~~0

range10 :: Range Int
range10 = (-8)>~0

range11 :: Range Int
range11 = (-8)~<0

range12 :: Range Int
range12 = (-8)><0

range13 :: Range Int
range13 = 0~~9

range14 :: Range Int
range14 = 0~<9

range15 :: Range Int
range15 = 0>~9

range16 :: Range Int
range16 = 0><9

spec :: Spec
spec = do
  describe "Unit tests" $ do

    describe "Endpoints" $ do
      describe "values" $ do
        it "value E (x, _) is x" $ do
          value endpoint1 `shouldBe` 1

      describe "extremes" $ do
        it "extreme E (_, Open) is Open" $ do
          extreme endpoint1 `shouldBe` Open

    describe "Ranges" $ do
      describe "intersection" $ do
        it ("The intersection between " ++ show range1 ++ " " ++
          show range2 ++ "is" ++ show range1) $ do
          intersection range1 range2 `shouldBe` range1

main :: IO ()
main = hspec spec
