module BinaryHeap(empty, insert, minimum, extractMin) where

import Prelude hiding (minimum)

data BinHeap a = Empty
               | Node Bool (BinHeap a) a (BinHeap a)
               deriving (Show)

empty :: BinHeap a
empty = Empty

insert :: (Ord a) => a -> BinHeap a -> BinHeap a
insert x Empty = Node True Empty x Empty
insert x (Node True  h1 y h2) = Node False (insert (max x y) h1) (min x y) h2
insert x (Node False h1 y h2) = Node True h1 (min x y) (insert (max x y) h2)

minimum :: BinHeap a -> a
minimum (Node _ _ x _) = x

extractMin :: Ord a => BinHeap a -> (a, BinHeap a)
extractMin Empty                  = error "Cannot extract minimum from an empty heap"
extractMin (Node _ Empty x Empty) = (x, Empty)
extractMin (Node True h1 x h2)    =
  let (y,h2') = extractMin h2
      (z,h1') = siftDown y h1
   in (x, Node False h1' z h2')
extractMin (Node False h1 x h2)   =
  let (y,h1') = extractMin h1
      (z,h2') = siftDown y h2
   in (x, Node True h1' z h2')

siftDown :: Ord a => a -> BinHeap a -> (a, BinHeap a)
siftDown x Empty = (x, Empty)
siftDown x h@(Node b h1 y h2)
  | x > y = (y, downHeap $ Node b h1 x h2)
  | otherwise = (x, h)

downHeap :: Ord a => BinHeap a -> BinHeap a
downHeap (Node b h1 x h2) =
  if minRoot h1 h2
  then let (x',h1') = siftDown x h1
       in (Node b h1' x' h2)
  else let (x',h2') = siftDown x h2
       in (Node b h1 x' h2')
downHeap h = h

minRoot :: Ord a => BinHeap a -> BinHeap a -> Bool
minRoot (Node _ _ x _) (Node _ _ y _) = x <= y
minRoot _ Empty = True
minRoot _ _     = False
