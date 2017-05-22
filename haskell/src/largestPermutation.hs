module LargestPermutation where

import Control.Applicative
import Control.Monad
import System.IO
import Data.Functor
import Data.List

countSwaps :: [Int] -> [Int] -> Int
countSwaps original swapped = let
  zipped = zip original swapped
  diff = filter (\(i, j) -> i /= j) zipped
  in length diff

-- http://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

permut :: Int -> [Int] -> [([Int], Int)]
permut k a = let
  permuts = permutations a
  -- permutCounts = sort $ filter (<= k) $ fmap (countSwaps a) permuts
  permutCounts = fmap (countSwaps a) permuts
  putmutsCounted = sortOn snd (zip permuts permutCounts)
  putmutCountsFiltered = reverse $ filter (\(_, c) -> c <= k) putmutsCounted :: [([Int], Int)]
  putmutCountsSorted = reverse $ sortOn (fromDigits . fst) putmutCountsFiltered
  in putmutCountsSorted

main :: IO ()
main = do
  nk' <- getLine
  let n = (read [(nk' !! 0)]) :: Int
      k = (read [(nk' !! 2)]) :: Int
  a_temp <- getLine
  let a = map read $ words a_temp :: [Int]
      best = fst $ head $ permut (k+1) a

  forM_ best (\i -> putStr ((show i) ++ " "))
      
    

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          
