module RedBlackTree where

data Color = R | B

data Tree a = E | T Color (Tree a) a (Tree a)

-- Invariants
-- 1. No red node has a red parent
-- 2. Every path from the root node to an empty node contains the same number of black nodes


