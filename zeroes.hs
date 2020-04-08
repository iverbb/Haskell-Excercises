module Zeroes where

import Data.List

factorize :: Integral a => a -> a -> [a]
factorize 0 _ = []
factorize 1 _ = []
factorize 2 _ = [2]
factorize n m
  | m > ceiling (sqrt $ fromIntegral n)
  = [n]
  | b == 0
  = [m] <> factorize a m
  | otherwise
  = factorize n (m+1)
  where (a,b) = divMod n m


--multipliers :: Integral a => a -> ([a], [a])
multipliers n = (nub fs, length <$> (group fs))
  where fs = factorize n 2

--allExps :: Integral a => a -> a -> a -> a
allExps n f c = sum [div n (f^x) | x <- [1..c]]


--zeroes :: Integral a => a -> a -> a
zeroes 0 _ = 0
zeroes b n = minimum $ zipWith (div) (zipWith (allExps n) fs cs) cs
	where (fs, cs) = multipliers b
       
