module Queue where

import Prelude hiding (head)

data Queue a = Queue [a] [a] deriving Show

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

(>>|) :: a -> Queue a -> Queue a
(>>|) x (Queue front rear) = Queue front (x:rear)

head :: Queue a -> Maybe a
head (Queue [] []) = Nothing
head (Queue [] rear) = head (Queue (reverse rear) [])
head (Queue (x:xs) _) = Just x


(|>>) :: Queue a -> (Maybe a, Queue a)
(|>>) q@(Queue [] []) = (Nothing, q)
(|>>) (Queue [] rear) = (|>>) (Queue (reverse rear) [])
(|>>) (Queue (x:xs) rear) = (Just x , Queue xs rear)
