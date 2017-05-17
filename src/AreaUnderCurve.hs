module AreaUnderCurve where


import Control.Applicative
import Control.Monad
import System.IO

import Data.List

import Text.Printf (printf)

n :: Double
n = 100000.0

term :: Int -> Int -> Double -> Double
term coef exp x = (fromIntegral coef) * (x**(fromIntegral exp))

f :: [Int] -> [Int] -> Double -> Double
f as bs x = let
  asbs = zip as bs
  in foldr (\(a, b) s -> s + term a b x) 0.0 asbs

cylinder :: Double -> Double -> Double
cylinder step r = step*pi*(r**2.0)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = let
  step = fromIntegral (r - l) / n
  xs = [(fromIntegral l), (fromIntegral l)+step .. (fromIntegral r)]
  ys = fmap (f a b) xs
  as = fmap (\y -> y*step) ys
  area = foldl (+) 0.0 as
  vs = fmap (cylinder step) ys
  volume = foldl (+) 0.0 vs
  in [area, volume]
  

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
