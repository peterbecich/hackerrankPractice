module MinimumAbsoluteDifference where

import Control.Applicative
import Control.Monad
import System.IO
import Data.List

diffs :: [Int] -> [Int]
diffs a = do
  x <- a
  y <- delete x a
  pure $ abs $ x - y
       

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
    error "fo"

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          
