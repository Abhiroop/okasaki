module Deque where

import Prelude hiding (head,last)

-- A double ended queue

data Deque a = Deque [a] [a] deriving Show

empty :: Deque a
empty = Deque [] []

isEmpty :: Deque a -> Bool
isEmpty (Deque [] []) = True
isEmpty _             = False

cons :: a -> Deque a -> Deque a
cons x (Deque front rear) = Deque (x:front) rear

snoc :: a -> Deque a -> Deque a
snoc x (Deque front rear) = Deque front (x:rear)

head :: Deque a -> Maybe a
head (Deque [] []) = Nothing
head (Deque [] rear) = head $ Deque (reverse h2) h1
                       where (h1, h2) = splitHalf rear
head (Deque (x:xs) _) = Just x

last :: Deque a -> Maybe a
last (Deque [] []) = Nothing
last (Deque front []) = last $ Deque h1 (reverse h2)
                        where (h1, h2) = splitHalf front
last (Deque _ (x:xs)) = Just x

extractFirst :: Deque a -> (Maybe a, Deque a)
extractFirst q@(Deque [] []) = (Nothing, q)
extractFirst (Deque [] rear) = extractFirst $ Deque (reverse h2) h1
                               where (h1, h2) = splitHalf rear
extractFirst (Deque (x:xs) rear) = (Just x, Deque xs rear)

extractLast :: Deque a -> (Maybe a, Deque a)
extractLast q@(Deque [] []) = (Nothing, q)
extractLast (Deque front []) = extractLast $ Deque h1 (reverse h2)
                               where (h1, h2) = splitHalf front
extractLast (Deque front (x:xs)) = (Just x, Deque front xs)

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

