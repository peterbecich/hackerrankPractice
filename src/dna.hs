module DNA where

import Control.Applicative
import Control.Monad
import System.IO


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    genes_temp <- getLine
    let genes = words genes_temp
    health_temp <- getLine
    let health = map read $ words health_temp :: [Int]
    s_temp <- getLine
    let s = read s_temp :: Int
    forM_ [1..s] $ \a0  -> do
        first_temp <- getLine
        let first_t = words first_temp
        let first = read $ first_t!!0 :: Int
        let last = read $ first_t!!1 :: Int
        let d = first_t!!2
        -- your code goes here


getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          
