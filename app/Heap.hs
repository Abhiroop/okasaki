{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
module Heap where

import Data.List.Ordered

class Heap t a where
    empty :: t a
    isEmpty :: t a -> Bool
    insert :: a -> t a -> t a
    minimum :: t a -> a
    extract :: t a -> (a, t a)


instance Ord a => Heap [] a where
  empty          = []
  isEmpty []     = True
  isEmpty (x:xs) = False
  insert x h     = x : h
  minimum        = Prelude.minimum
  extract h      = let min = Heap.minimum h
                    in (min, removeItem min h)

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys)
  | x == y = removeItem x ys
  | otherwise = y : removeItem x ys

newtype OrderedList a = OrderedList ([a])

unwrap :: OrderedList a -> [a]
unwrap (OrderedList x) = x

instance Ord a => Heap OrderedList a where
  empty                  = OrderedList []
  isEmpty (unwrap -> []) = True
  isEmpty _              = False
  insert x l             = OrderedList $ insertBag x (unwrap l)
  minimum l              = head $ unwrap l
  extract l              = (Heap.minimum l, OrderedList $ drop 1 (unwrap l))
