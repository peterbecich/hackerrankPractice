module RadioTransmitter where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import System.IO
import Data.List
import Data.Functor
import Data.Maybe

-- https://www.hackerrank.com/challenges/hackerland-radio-transmitters/forum

inRange :: Int -> Int -> [Int] -> [Int]
inRange transmitterRange transmitterPlacement houses =
  filter (\h -> abs (h - transmitterPlacement) <= transmitterRange) houses

filterInRange :: Int -> Int -> [Int] -> [Int]
filterInRange transmitterRange transmitterPlacement houses = let
  ir = inRange transmitterRange transmitterPlacement houses
  in filter (flip notElem ir) houses
  
numInRange :: Int -> Int -> [Int] -> Int
numInRange transmitterRange transmitterPlacement houses =
  length $ inRange transmitterRange transmitterPlacement houses

greedyTransmitterPlacement :: Int -> [Int] -> Maybe (Int, [Int])
greedyTransmitterPlacement transmitterRange houses = do
  -- minPlacement = minimum houses
  -- maxPlacement = maximum houses
  let inRangeCounts = zip houses $ fmap (\p -> numInRange transmitterRange p houses) houses
  (optimalPlacement, numCovered) <- fst <$> (uncons (sortOn snd inRangeCounts))
  let outOfRange = filterInRange transmitterRange optimalPlacement houses
  return $ (optimalPlacement, outOfRange)


foldfunc :: Int -> Int -> ([Int], [Int]) -> ([Int], [Int])
foldfunc _ _ (transmitters, []) = (transmitters, [])
foldfunc transmitterRange _ (transmitters, houses) = let
  maybePlacement = greedyTransmitterPlacement transmitterRange houses
  in case maybePlacement of
    (Just (placement, remainingHouses)) -> (placement : transmitters, remainingHouses)
    (Nothing) -> (transmitters, houses)

main :: IO ()
main = do
    n_temp <- getLine
    let n_t = words n_temp
    let n = read $ n_t!!0 :: Int -- number of houses
    let range = read $ n_t!!1 :: Int -- range of individual transmitter
    x_temp <- getLine
    let x = map read $ words x_temp :: [Int] -- positions of houses
        houses = sort $ intersect x x  -- distinct houses
        placements = fst $ foldr (foldfunc range) ([], houses) [1..(length houses)]
    putStrLn $ show $ length placements - 1
    

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          

-- incorrect on most test cases
