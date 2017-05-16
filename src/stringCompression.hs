module StringCompression where

import           Control.Monad
import           Data.List

repetitions :: String -> [(String, Int)] -> [(String, Int)]
repetitions nextChar ((prevChar, n):ll)
    | nextChar == prevChar = (nextChar, n+1):ll
    | otherwise = (nextChar,1):((prevChar,n):ll)

count :: String -> [(String, Int)]
count s = foldr repetitions [("",1)] $ fmap (\c -> [c]) s

flattenFold :: (String, Int) -> String -> String
flattenFold (nextChar, nextCount) out
    | nextCount <= 1 = nextChar ++ out
    | otherwise = nextChar ++ (show nextCount) ++ out

compress :: String -> String
compress s = let
    counts = count s :: [(String, Int)]
    flattened = foldr flattenFold "" counts :: String
    in flattened


main :: IO ()
main = do
  uncompressed <- getLine
  let compressed = compress uncompressed
  putStrLn compressed


example3 = "aaabaaaaccaaaaba"
counted = count example3
compressed = compress example3
