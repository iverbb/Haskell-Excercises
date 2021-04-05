module ColorContrast where

import Control.Lens
import Numeric
import Data.List.Split
import Data.Bool

-- 8-bit color normalization
normalize :: Int -> Float
normalize = (/ 255) . fromIntegral

-- Smoothen out perceived color spectrum
-- Perceived light is a concave function of intensity
gammaCorrect :: Float -> Float
gammaCorrect = bool 
  <$> ((** 2.4) . (/ 1.055) . (+ 0.055)) 
  <*> (/ 12.92)
  <*> (<= 0.03928)

-- Adjust for variance in human eye sensitivty to red, green, and blue
colorWeight :: [Float] -> [Float]
colorWeight = zipWith (*) [ 0.2126, 0.7152, 0.0722 ]

-- Calculate the perceived overall brightness
luminosity :: [Int] -> Float
luminosity = sum . colorWeight . over each  (gammaCorrect . normalize)

-- Contrast two luminosities
colorContrast :: [Int] -> [Int] -> Float
colorContrast color1 color2
  = (bool <$> id <*> (** (-1)) <*> (< 1)) contrast
    where [l1, l2] = luminosity <$> [color1, color2]
          contrast = (l1 + 0.05) / (l2 + 0.05)

-- Read hexinput
fromHex :: String -> [Int]
fromHex ('0' : '#' : xs) = fromHex xs
fromHex x = chunksOf (length x `div` 3) x >>= (fst <$>) . readHex

-- End to end computation of hexcolor to contrast
contrastFromHex :: String -> String -> Float
contrastFromHex x y = colorContrast x' y'
   where [x', y'] = fromHex <$> [x, y]

-- Usage: main hexcolor hexcolor
-- Output: contrast ratio
-- Won't crash with strings of different length than 6, but won't
-- give reasonable results
main :: String -> String -> IO ()
main x y = print $ contrastFromHex x y

-- Usage and Warnings as above
-- Output WCAG compliance rating for normal text
complianceRating :: String -> String -> IO ()
complianceRating x y = putStrLn code
  where contrast = contrastFromHex x y
        code
          | contrast > 7   = "AAA"
          | contrast > 4.5 = "AA"
          | otherwise      = "A"
    


