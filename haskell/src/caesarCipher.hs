module CaesarCipher where

import Control.Applicative
import Control.Monad
import System.IO

import Data.Functor (fmap)
import Data.Char (ord)
import Data.Maybe
import Data.List

upperLetters = ['A'..'Z']
lowerLetters = ['a'..'z']

getUpperLetter :: Int -> Char
getUpperLetter i = lowerLetters !! (i `mod` length upperLetters)

getUpperIndex :: Char -> Maybe Int
getUpperIndex c = elemIndex c upperLetters

getLowerLetter :: Int -> Char
getLowerLetter i = lowerLetters !! (i `mod` length lowerLetters)

getLowerIndex :: Char -> Maybe Int
getLowerIndex c = elemIndex c lowerLetters

isUpper :: Char -> Bool
isUpper c = elem c upperLetters

isLower :: Char -> Bool
isLower c = elem c lowerLetters

shiftLetter :: Int -> Char -> Char
shiftLetter k c
  | isUpper c = getUpperLetter (i + k)
  | isLower c = getLowerLetter (i + k)
  | otherwise = c
  where i = ord c

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    s <- getLine
    k_temp <- getLine
    let k = read k_temp :: Int
        cipher = fmap (shiftLetter k) s
    putStrLn cipher
    

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          

