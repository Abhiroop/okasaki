module Deque where

-- A double ended queue

data Deque a = Deque [a] [a] deriving Show

empty :: Deque a
empty = Deque [] []

isEmpty :: Deque a
isEmpty = undefined

cons :: a -> Deque a -> Deque a
cons = undefined

snoc :: a -> Deque a -> Deque a
snoc = undefined

head :: Deque a -> Maybe a
head = undefined

last :: Deque a -> Maybe a
last = undefined

extractFirst :: Deque a -> (Maybe a, Deque a)
extractFirst = undefined

extractLast :: Deque a -> (Maybe a, Deque a)
extractLast = undefined


