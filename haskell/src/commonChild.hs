module CommonChild where

import Control.Applicative
import Control.Monad
import System.IO

import Data.List

zipper xs' ys' = let
  xys = zip xs' ys'
  xys' = filter (\(x, y) -> x == y) xys
  -- s = fmap fst xys'
  in length xys'

orderedIntersect :: String -> String -> Int
orderedIntersect xs ys = let
  intersection = intersect xs ys
  xs' = filter (\x -> elem x intersection) xs
  ys' = filter (\y -> elem y intersection) ys
  intersectionCounts :: [[Int]]
  intersectionCounts = forM [0..(length xs')] (\i -> let
                             shift = replicate i ' '
                             ys'' = shift ++ ys'
                             in pure $ zipper xs' ys''
                         )
  intersectionCounts' :: [Int]
  intersectionCounts' = concat intersectionCounts
  in maximum intersectionCounts'
                            


main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  let orderedIntersection = orderedIntersect s1 s2

  putStrLn $ show $ orderedIntersect s1 s2
