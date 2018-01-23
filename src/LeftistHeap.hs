module LeftistHeap where

data LHeap a = Empty
             | Node Int a (LHeap a) (LHeap a)

empty :: LHeap a
empty = Empty
