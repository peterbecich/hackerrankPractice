module IceCreamParlor where

import Control.Applicative
import Control.Monad
import System.IO

import Data.List
-- import Data.Map
-- import Data.Map.Lazy

-- https://www.hackerrank.com/challenges/icecream-parlor
choose :: Int -> [(Int, Int)] -> (Int, Int)
choose budget iceCreams = undefined

main :: IO ()
main = do
  t' <- getLine
  let t = read t' :: Int
  forM_ [1..t] (\_ -> do
                  m' <- getLine
                  let m = read m' :: Int
                  n' <- getLine
                  let n = read n' :: Int
                  prices' <- getLine
                  let prices = map read $ words prices' :: [Int]
                      indexedPrices = reverse $ sortOn snd $ zip [1..] prices
                      -- iceCreams = foldr (\(i, p) m -> insert i p m) empty indexedPrices
                      picks = choose m indexedPrices
                  putStrLn $ (fst picks) ++ " " ++ (snd picks)
               )
  
