module BinaryHeap where

data BinHeap a = Empty
               | Node Bool (BinHeap a) a (BinHeap a)
               deriving (Show)

insert :: (Ord a) => a -> BinHeap a -> BinHeap a
insert x Empty = Node True Empty x Empty
insert x (Node True  h1 y h2) = Node False (insert (max x y) h1) (min x y) h2
insert x (Node False h1 y h2) = Node True h1 (min x y) (insert (max x y) h2)

minimum :: BinHeap a -> a
minimum (Node _ _ a _) = a
