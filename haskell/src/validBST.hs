module ValidBST where

-- https://en.wikipedia.org/wiki/Tree_traversal#Pre-order

-- data Tree = Node Tree Tree | Leaf

-- foo = Node (Node Leaf Leaf) Leaf

-- https://dkalemis.wordpress.com/2014/01/23/trees-in-haskell/
data Tree a = Nil | Node a (Tree a) (Tree a)

foo = Node 1 (Node 3 Nil Nil) (Node 2 Nil Nil)

insertElement :: (Ord a, Eq a) => Tree a -> a -> Tree a
insertElement Nil x = Node x Nil Nil
insertElement t@(Node y left right) x
    | x < y = Node y (insertElement left x) right
    | x > y = Node y left (insertElement right x)
    | otherwise = t

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f z Nil = z
    foldr f z (Node a left right) = 
        
