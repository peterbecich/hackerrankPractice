module MaxSubarray where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import System.IO
import Data.List
import Data.Functor
import Data.Maybe


stocks = [100, 113, 110, 85, 105, 102, 86, 63, 81, 101, 94, 106, 101, 79, 94, 90, 97]
stockDiffs = tail $ fmap (\(i, j) -> i - j) $ zip stocks (0:stocks)

diffs2 = [0, 1, 2, 3, 30, -30, 60]


findMaxCrossingSubarray :: [Int] -> Int -> Int -> Int -> (Int, Int, Int)
findMaxCrossingSubarray arr low mid high = let
  (cumulativeLeftSum, maxLeftSum, maxLeftPosition) =
    foldr (\i (s, maxLeftSum, maxLeftPosition) -> let
            s' = (arr !! i) + s
            in if (s' > s)
               then (s', s', i)
               else (s', maxLeftSum, maxLeftPosition)
        ) (0, minBound::Int , mid) (reverse [low..mid])
  (cumulativeRightSum, maxRightSum, maxRightPosition) =
    foldr (\i (s, maxRightSum, maxRightPosition) -> let
            s' = (arr !! i) + s
            in if (s' > s)
               then (s', s', i)
               else (s', maxRightSum, maxRightPosition)
        ) (0, minBound::Int, mid+1) [mid+1 .. high]
  -- max left, max right, max combined
  in (maxLeftPosition, maxRightPosition, maxLeftSum + maxRightSum)


--                                        (left , right, sum)
findMaxSubarray :: [Int] -> Int -> Int -> (Int, Int, Int)
findMaxSubarray arr low high
  | low == high = (low, high, arr !! low)
  | otherwise = let
      mid = floor (fromIntegral (low + high) / 2) :: Int
      (leftLow, leftHigh, leftSum) = findMaxSubarray arr low mid
      (rightLow, rightHigh, rightSum) = findMaxSubarray arr (mid+1) high
      (crossLow, crossHigh, crossSum) =
        findMaxCrossingSubarray arr low mid high
      in if (leftSum >= rightSum && leftSum >= crossSum)
        then (leftLow, leftHigh, leftSum)
      else if (rightSum >= leftSum && rightSum >= crossSum)
        then (rightLow, rightHigh, rightSum)
      else (crossLow, crossHigh, crossSum)


maxStockSubarray = findMaxSubarray stockDiffs 0 (length stockDiffs - 1)
maxSubarray2 = findMaxSubarray diffs2 0 (length diffs2 - 1)
    
  

