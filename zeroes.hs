module Zeroes where

import Data.List (genericLength, group, nub)

factorize :: Integral a => a -> [a]
factorize n 
  = case f of
      [] -> [n]
      _  -> f <> factorize (div n (head f))
  where f = take 1 $ filter (\x -> mod n x == 0) [2..n-1]

multipliers :: Integral a => a -> ([a], [a])
multipliers n = (nub fs, genericLength <$> group fs)
  where fs = factorize n

allExps :: Integral a => a -> a -> a -> a
allExps n f c = sum [div n (f^x) | x <- [1..c]]


zeroes :: Integral a => a -> a -> a
zeroes 0 _ = 0
zeroes b n = minimum $ zipWith (div) (zipWith (allExps n) fs cs) cs
	where (fs, cs) = multipliers b
       
