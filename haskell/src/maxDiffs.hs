module MaxDiffs where

import Control.Applicative
import Control.Monad
import System.IO
import Data.Functor
import Data.List

cross = do
  i <- [1..4] :: [Int]
  j <- [1..4] :: [Int]
  pure (i*j)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
        
    putStrLn $ show $ maxdiff a

maxdiff :: [Int] -> Int
maxdiff arr = let
  (lowest, highest) = foldl foldF (maxBound::Int, minBound::Int) arr
  in abs (highest-lowest)

foldF :: (Int, Int) -> Int -> (Int, Int)
foldF (lowest, highest) x
  | x < lowest = (x, highest)
  | x > highest = (lowest, x)
  | otherwise = (lowest, highest)

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          
