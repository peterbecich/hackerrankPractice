module HackerrankString where

import Control.Applicative
import Control.Monad
import System.IO

hackerrank = "hackerrank"

-- target: hackerrank
-- test: searching for hackerrank in this string
zipper :: String -> String -> Bool
zipper [] _ = True -- all characters of hackerrank matched
zipper (_:_) [] = False -- test input exhausted
zipper target@(x:xs) (y:ys)
  | x == y = zipper xs ys
  | otherwise = zipper target ys

main :: IO ()
main = do
    q_temp <- getLine
    let q = read q_temp :: Int  -- number of queries
    forM_ [1..q] $ \a0  -> do
        s <- getLine
        -- your code goes here
        let zips = zipper hackerrank s
        if zips
          then putStrLn "YES"
          else putStrLn "NO"


getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret       
