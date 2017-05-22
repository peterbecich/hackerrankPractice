module RodCutting where

import Control.Applicative
import Control.Monad
import System.IO

import Data.List
import qualified Data.Map.Lazy as Map
import Data.Int

takePrices :: [Int] -> Map.Map Int Int
takePrices arr = Map.fromList $ zip [1..] arr

rodPrices1 = takePrices [1, 5, 8, 9, 10, 17, 17, 20, 24, 30]

naiveFold :: Map.Map Int Int -> Int -> Int -> ([Int], Int) -> ([Int], Int)
naiveFold prices n i (lengths, q) = let
  price = (Map.!) prices i  -- unsafe
  p = price + snd (cutRodNaive prices (n - i)) -- need lengths arr from subroutine
  in if (p > q)
    then (i : lengths, p)
    else (lengths, q)

cutRodNaive :: Map.Map Int Int
                -> Int
                -> ([Int], Int)  -- cuts, profit
cutRodNaive prices rodLength
  | rodLength == 0 = ([], 0)
  | otherwise =
      foldr (naiveFold prices rodLength) ([], minBound :: Int) [1..rodLength]


-- naiveFoldMemo :: Map.Map Int Int
--               -> Int
--               -> Int
--               -> Map.Map Int [Int]
--               -> Map.Map Int [Int]              
-- naiveFoldMemo prices n i cuts = let
--   price = (Map.!) prices i  -- unsafe
--   p = price + snd (cutRodNaiveMemo prices (n - i)) -- need lengths arr from subroutine
--   in if (p > q)
--     then (i : lengths, p)
--     else (lengths, q)

-- cutRodNaiveMemo :: Map.Map Int Int
--                 -> Int
--                 -> Map.Map Int [Int]
--                 -> Map.Map Int [Int]
-- cutRodNaiveMemo prices rodLength cuts
--   | rodLength == 0 = Map.empty
--   | otherwise =
--       foldr (naiveFoldMemo prices rodLength cuts) ([], minBound :: Int) [1..rodLength]
