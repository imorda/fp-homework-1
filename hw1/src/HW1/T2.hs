module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Data.Maybe (fromJust)
import Numeric.Natural

data N = Z | S N deriving (Show)

nplus :: N -> N -> N
nplus a Z     = a
nplus a (S b) = nplus (S a) b

nmult :: N -> N -> N
nmult _ Z     = Z
nmult a (S b) = nplus a $ nmult a b

nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub Z _         = Nothing
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering
ncmp a b = case a `nsub` b of
  Nothing -> LT
  Just Z  -> EQ
  _       -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S $ nFromNatural $ x - 1

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S x) = 1 + nToNum x

nEven :: N -> Bool
nEven Z     = True
nEven (S x) = not $ nEven x

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv Z _ = Z
-- Tried implementing as inline "S . (`ndiv` b)" but concluded that to be much less readable
ndiv a b = maybe Z divByBAndIncrement (a `nsub` b)
  where
    divByBAndIncrement y = S $ y `ndiv` b

nmod :: N -> N -> N
nmod a b = fromJust $ a `nsub` (b `nmult` (a `ndiv` b))
