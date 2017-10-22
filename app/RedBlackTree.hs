module RedBlackTree where

data Color = R | B

data Tree a = E | T Color (Tree a) a (Tree a)

-- Invariants
-- 1. No red node has a red parent
-- 2. Every path from the root node to an empty node contains the same number of black nodes

-- | Simple Set operations
type Set a = Tree a

empty :: Set a
empty = E

member :: (Ord a) => a -> Set a -> Bool
member x E    = False
member x (T _ a y b)
  | x < y     = member x a
  | x == y    = True
  | otherwise = member x b

-- | Insertion
insert :: (Ord a) => a -> Set a -> Set a
insert x s = makeBlack $ ins s
  where ins E  = T R E x E
        ins (T color a y b)
          | x < y  = balance color (ins a) y b
          | x == y = T color a y b
          | x > y  = balance color a y (ins b)
        makeBlack (T _ a y b) = T B a y b

balance :: Color -> Set a -> a -> Set a -> Set a
balance = undefined



