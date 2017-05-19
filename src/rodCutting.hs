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

naiveFold :: Map.Map Int Int -> Int -> ([Int], Int) -> ([Int], Int)
naiveFold prices l (lengths, q) = let
  price = (Map.!) prices l  -- unsafe
  p = price + (cutRodNaive prices (rodLength - l)) -- need lengths arr from subroutine
  in if (p > q)
    then (l : lengths, p)
    else (lengths, q)

cutRodNaive :: Map.Map Int Int -> Int -> ([Int], Int)
cutRodNaive prices rodLength
  | _ 0 = ([], 0)
  | otherwise = foldr (naiveFold prices) ([], minBound :: Int) [1..rodLength]
