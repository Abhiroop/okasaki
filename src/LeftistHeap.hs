module LeftistHeap(empty, union, insert, extract, extractMin) where

data LHeap a = Empty
             | Node Int a (LHeap a) (LHeap a)

empty :: LHeap a
empty = Empty

{- INVARIANTS
1. Every node is smaller or equal to its children
2. The rank of the left child is >= to the rank of right child
-}

rank :: LHeap a -> Int
rank Empty          = 0
rank (Node x _ _ _) = x

union :: Ord a => LHeap a -> LHeap a -> LHeap a
union h1 Empty = h1
union Empty h2 = h2
union h1@(Node _ x1 h11 h12) h2@(Node _ x2 h21 h22)
  | x1 <= x2  = makeLH x1 h11 (union h12 h2)
  | otherwise = makeLH x2 h21 (union h1 h22)

-- This function repairs the leftist propery and creates a leftist tree/heap from a key and 2 children
makeLH :: a -> LHeap a -> LHeap a -> LHeap a
makeLH x h1 h2
  | rank h1 >= rank h2 = Node ((rank h2) + 1) x h1 h2
  | otherwise          = Node ((rank h1) + 1) x h2 h1

insert :: Ord a => a -> LHeap a -> LHeap a
insert x h = union (Node 1 x Empty Empty) h

extractMin :: LHeap a -> a
extractMin Empty          = error "Cannot extract min from an empty heap"
extractMin (Node _ x _ _) = x

extract :: Ord a => LHeap a -> (a, LHeap a)
extract Empty            = error "Cannot extract min from an empty heap"
extract (Node _ x h1 h2) = (x, union h1 h2)
