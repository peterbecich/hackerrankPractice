module LoopTests where

import Control.Monad
import Control.Monad.Loops
import Data.List
import Data.Maybe

-- https://hackage.haskell.org/package/monad-loops-0.4.3/docs/Control-Monad-Loops.html

lessThan100 = iterateUntilM (> 100) (\i -> [i+1]) 0

-- http://www.haskellforall.com/2012/01/haskell-for-c-programmers-for-loops.html

-- whileM_
-- when :: Applicative f => Bool -> f () -> f ()
while :: (Monad m) => m Bool -> m a -> m ()
while condition action = do
  c <- condition
  when c $ do
    _ <- action
    while condition action

-- forM :: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
-- forM_ :: (Monad m, Foldable t) => t a -> (a -> m b) -> m ()

for :: (Monad m) => m a -> m Bool -> m b -> m c -> m ()
for init condition post action = do
  _ <- init
  while condition $ do
    _ <- action
    post


    
