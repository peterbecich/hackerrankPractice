module ConcavePolygon where

import Control.Applicative
import Control.Monad
import System.IO

import Data.List


main :: IO ()
main = do
  n' <- getLine
  let n = read n' :: Int
  points <- forM [1..n] (\_ -> do
                            l <- getLine
                            let x' = l !! 0
                                y' = l !! 2
                                x = read [x'] :: Int
                                y = read [y'] :: Int
                            pure (x, y)
                        )
  error "foo"
  
