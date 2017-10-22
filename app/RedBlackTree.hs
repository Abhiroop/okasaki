module RedBlackTree where

data Color = R | B

data Tree a = E | T Color (Tree a) a (Tree a)
