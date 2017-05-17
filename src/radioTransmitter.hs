module RadioTransmitter where

import Control.Applicative
import Control.Monad
import System.IO

-- https://www.hackerrank.com/challenges/hackerland-radio-transmitters/forum


main :: IO ()
main = do
    n_temp <- getLine
    let n_t = words n_temp
    let n = read $ n_t!!0 :: Int -- number of houses
    let k = read $ n_t!!1 :: Int -- range of individual transmitter
    x_temp <- getLine
    let x = map read $ words x_temp :: [Int] -- positions of houses

    error "foo"

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          

