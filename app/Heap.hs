{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
module Heap where

import Data.List.Ordered

class Heap t a where
    empty :: t a
    isEmpty :: t a -> Bool
    insert :: a -> t a -> t a
    minimum :: t a -> Maybe a
    extract :: t a -> (Maybe a, t a)


instance Ord a => Heap [] a where
  empty          = []
  isEmpty []     = True
  isEmpty (x:xs) = False
  insert x h     = x : h
  minimum []     = Nothing
  minimum  l     = Just $ Prelude.minimum l
  extract h      = let min = Heap.minimum h
                    in (min, removeItem min h)

removeItem :: Eq a => Maybe a -> [a] -> [a]
removeItem _ []      = []
removeItem Nothing l = l
removeItem (Just x) (y:ys)
  | x == y = removeItem (Just x) ys
  | otherwise = y : removeItem (Just x) ys

newtype OrderedList a = OrderedList ([a]) deriving Show

unwrap :: OrderedList a -> [a]
unwrap (OrderedList x) = x

instance Ord a => Heap OrderedList a where
  empty                  = OrderedList []
  isEmpty (unwrap -> []) = True
  isEmpty _              = False
  insert x l             = OrderedList $ insertBag x (unwrap l)
  minimum (unwrap -> []) = Nothing
  minimum l              = Just $ head $ unwrap l
  extract l              = (Heap.minimum l, OrderedList $ drop 1 (unwrap l))


listHeap :: Ord a => [a] -> [a] -> [a]
listHeap []  h   = h
listHeap (x:xs) h = listHeap xs (insert x h)

heapList :: Ord a => [a] -> [a]
heapList h = let (maybeMin,rest) = extract h
              in case maybeMin of
                   Nothing -> []
                   Just min -> min : heapList rest

heapSortUnorderedList :: Ord a => [a] -> [a]
heapSortUnorderedList l = heapList $ listHeap l []

listHeapOrderedList :: Ord a => [a] -> OrderedList a -> OrderedList a
listHeapOrderedList [] l     = l
listHeapOrderedList (x:xs) l = listHeapOrderedList xs $ insert x l

heapSortOrderedList :: Ord a => [a] -> [a]
heapSortOrderedList l = unwrap $ listHeapOrderedList l (OrderedList [])
