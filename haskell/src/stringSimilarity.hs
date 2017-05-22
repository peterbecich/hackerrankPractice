module StringSimilarity where

import Control.Applicative
import Control.Monad
import System.IO

import Data.List


zipper xs' ys' = let
  xys = zip xs' ys'
  xys' = filter (\(x, y) -> x == y) xys
  -- s = fmap fst xys'
  in length xys'

-- https://www.reddit.com/r/haskell/comments/1psdai/is_there_a_foldwhile_function_in_haskell/
-- Enter your code here. Read input from STDIN. Print output to STDOUT
similarity xs ys = let
  xys = zip xs ys
  in fst $ (foldl foldfunc (0, True) . (takeWhile (\(x, y) -> x==y))) xys

foldfunc (n, b) (x, y)
  | x /= y = (n, False)
  | x == y && b = (n+1, b)
  | otherwise = (n, False)

-- if (not b)
--           then (n, b)
--           else if (x == y)
--                then (n+1, b)
--           else (n, False)
  
--Replace the question marks with the desired code.

suffixes "" = []
suffixes xs = [xs] ++ suffixes (tail xs) 

similarities xs = sum[similarity x1 y1 
                         | (x1, y1) <- [(x,y) | x <- [xs], y <- suffixes xs]]

main :: IO ()
main = do
  is <- getLine
  let i = read is :: Int
  forM_ [1..i] $ \_ -> do
    s <- getLine
    let sim = similarities s
    putStrLn $ show sim
