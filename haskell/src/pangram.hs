module Pangram where

import Control.Applicative
import Control.Monad
import System.IO
import Data.Char (toLower)
import Data.Functor (fmap)
import Data.Maybe
import Data.List

lowerAlphabet = ['a'..'z']


-- remove :: Char -> [Char] -> Maybe ([Char])
-- remove c ll
--   | elem c ll = Just $ delete c ll
--   | otherwise = Nothing

main :: IO ()
main = do
  s <- getLine
  let lowerS = fmap toLower s :: [Char]
  
      remainingAlphabet = foldl (flip delete) lowerAlphabet lowerS

  if (length remainingAlphabet > 0)
    then putStrLn "not pangram"
    else putStrLn "pangram"



getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret       
