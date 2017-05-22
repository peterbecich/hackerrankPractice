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
    --     cross = do
    --       i <- a
    --       j <- delete i a
    --       pure (i, j) -- slow list ++
    --     cross' = sortOn (\(i, j) -> abs (i - j)) cross
    -- putStrLn $ show $ (\(i, j) -> abs (i - j)) (head cross')
        a' = sort a
        b = (maxBound :: Int) : a'
        ab = zip a' b
        diffs = fmap (\(i, j) -> abs (i - j)) ab
    putStrLn $ show $ minimum diffs
    

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          
