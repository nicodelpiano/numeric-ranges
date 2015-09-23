-----------------------------------------------------------------------------
--
-- Module      :  Numeric.Ranges.Internal
-- Copyright   :  (c) 2015 Nicolas Del Piano
-- License     :  MIT
--
-- Maintainer  :  Nicolas Del Piano <ndel314@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Framework for working with numeric ranges.
--
-----------------------------------------------------------------------------

module Numeric.Ranges.Internal where

-- | Range
--
-- It represents a range from a to b, with information about endpoints.
-- Example: Rg (1, Closed) (2.4, Open) is equivalent to [1, 2.4)
data Range a = Rg !(Endpoint a) !(Endpoint a) | Empty
  deriving Eq

-- | Type for endpoints.
data Endpoint a = E (a, Extreme)
  deriving (Eq, Show)

-- | Data-type for including or excluding endpoints.
data Extreme = Open | Closed
  deriving (Eq, Show)

instance (Show a) => Show (Range a) where
  show Empty = "Empty"
  show (Rg (E (x, l)) (E (y, r))) =
    showLeftB l ++ show x ++ ", " ++ show y ++ showRightB r
    where
    showLeftB Closed = "["
    showLeftB Open   = "("

    showRightB Closed = "]"
    showRightB Open   = ")"

-- | Ord instance for Extreme
instance Ord Extreme where
  Open <= Closed = True

-- | Num instance for Endpoint a
instance (Num a, Ord a) => Num (Endpoint a) where
  el + er = (value el + value er, max $ extreme el $ extreme er)

  el - er = (value el - value er, min $ extreme el $ extreme er)

  el * er = (value el * value er, max $ extreme el $ extreme er)
-- | Num instance for Range a
--
-- re and le stand for right endpoint and left endpoint.
instance (Num a, Ord a) => Num (Range a) where
  Rg le re + Rg le' re' = Rg (le + le') (re' + re')
  _ + _ = Empty

  Rg le re - Rg le' re' = Rg (le - le') (re' - re')
  _ - _ = Empty

  Rg le re * Rg le' re' = Rg (minimum prods) (maximum prods)
    where
    prods = [le*le', le*re', re*le', re*re']

  abs r@(Rg le re)
    | le >= 0 = r
    | re <= 0 = negate r 
    | otherwise = 0 ~~ max (-le) re

infix 3 ><
infix 3 >~
infix 3 ~<
infix 3 ~~

-- | Constructs an open range, i.e., (a, b).
(><) :: (Num a, Ord a) => a -> a -> Range a
x >< y
  | x <= y = Rg (E (x, Open)) (E (y, Open))
  | otherwise = Empty

-- | Constructs a left-opened range, i.e., (a, b].
(>~) :: (Num a, Ord a) => a -> a -> Range a
x >~ y
  | x <= y = Rg (E (x, Open)) (E (y, Closed))
  | otherwise = Empty

-- | Constructs a right-open range, i.e., [a, b).
(~<) :: (Num a, Ord a) => a -> a -> Range a
x ~< y
  | x <= y = Rg (E (x, Closed)) (E (y, Open))
  | otherwise = Empty

-- | Constructs a closed range, i.e., [a, b].
(~~) :: (Num a, Ord a) => a -> a -> Range a
x ~~ y
  | x <= y = Rg (E (x, Closed)) (E (y, Closed))
  | otherwise = Empty

-- | Take the extreme value from an endpoint.
extreme :: Endpoint a -> Extreme
extreme (E (_, e)) = e

-- | Take the endpoint number value from an endpoint.
value :: Endpoint a -> a
value (E (x, e)) = x

-- | Is a closed endpoint?
isClosed :: Endpoint a -> Bool
isClosed = (Closed ==) . extreme

-- | Is an open endpoint?
isOpen :: Endpoint a -> Bool
isOpen = (Open ==) . extreme

-- | Positive infinity
inf :: (Fractional a) => a
inf = 1/0

-- | Negative infinity
ninf :: (Fractional a) => a
ninf = -inf

-- | The full range (-inf, inf)
full :: (Fractional a, Ord a) => Range a
full = ninf >< inf

-- | An empty range
empty :: (Fractional a, Ord a) => Range a
empty = Empty

-- | Degenerate.
--
-- degenerate [a, a] = a
degenerate :: (Num a, Ord a) => Range a -> Maybe a
degenerate (Rg (E (x, Closed)) (E (y, Closed)))
  | x == y = Just x
  | otherwise = Nothing
degenerate _ = Nothing

-- | A unitary range.
--
-- unitary a = [a, a]
unitary :: (Num a, Ord a) => a -> Range a
unitary x = x ~~ x

-- | Is the given range a unitary element?
isUnitary :: (Num a, Ord a) => Range a -> Bool
isUnitary (Rg (E (x, _)) (E (y, _))) = x == y
isUnitary _ = False

-- | Is the given range empty?
isEmpty :: (Num a, Ord a) => Range a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- | Range length.
--
-- range (a,b) = b - a 
range :: (Num a, Ord a) => Range a -> a
range Empty = 0 
range (Rg x y) = value y - value x

-- | The infimum of a range.
infimum :: (Num a, Ord a) => Range a -> a
infimum Empty = error "infimum: Empty range."
infimum (Rg x _) = value x

-- | The supreme of a range.
supreme :: (Num a, Ord a) => Range a -> a
supreme Empty = error "supreme: Empty range."
supreme (Rg _ y) = value y

-- | Is the given element present in the range?
--
-- As an example, consider:
--   isX 3 $ 1 >< 3   -> False
--   isX 3 $ 1 >~ 3   -> True
inX :: (Num a, Ord a) => a -> Range a -> Bool
inX _ Empty = False
inX x r = go
  where
  go x (Rg (l, Open) (r, Open)) = x > l && x < r
  go x (Rg (l, Closed) (r, Open)) = x >= l && x < r
  go x (Rg (l, Open) (r, Closed)) = x > l && x <= r
  go x (Rg (l, Closed) (r, Closed)) = x >= l && x <= r

-- | Is the given element not present in the range?
notX :: (Num a, Ord a) => a -> Range a -> Bool
notX = not . inX

-- | Builds a range from a given number.
--
-- Example:
--   rangeFrom 3 = [3, inf)
rangeFrom :: (Num a, Ord a) => a -> Range a
rangeFrom = (~<inf)

-- | Builds an open range from a given number.
openFrom :: (Num a, Ord a) => a -> Range a
openFrom = (><inf)
