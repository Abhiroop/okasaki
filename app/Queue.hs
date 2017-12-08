module Queue where

import Prelude hiding (head)

data Queue a = Queue [a] [a] deriving Show

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

snoc :: a -> Queue a -> Queue a
snoc x (Queue front rear) = Queue front (x:rear)

head :: Queue a -> Maybe a
head (Queue [] []) = Nothing
head (Queue [] rear) = head (Queue (reverse rear) [])
head (Queue (x:xs) _) = Just x


extract :: Queue a -> (Maybe a, Queue a)
extract q@(Queue [] []) = (Nothing, q)
extract (Queue [] rear) = extract (Queue (reverse rear) [])
extract (Queue (x:xs) rear) = (Just x , Queue xs rear)
