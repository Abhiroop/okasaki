module RedBlackTree where

data Color = R | B

data Tree a = E | T Color (Tree a) a (Tree a)

-- Invariants
-- 1. No red node has a red parent
-- 2. Every path from the root node to an empty node contains the same number of black nodes

-- Simple Set operations
type Set a = Tree a

empty :: Set a
empty = E

member :: (Ord a) => a -> Set a -> Bool
member x E    = False
member x (T _ a y b)
  | x < y     = member x a
  | x == y    = True
  | otherwise = member x b
