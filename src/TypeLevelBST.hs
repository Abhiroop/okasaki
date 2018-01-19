{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs, DataKinds  #-}
module BST where

data Nat = Zero | Succ Nat

data Tree n a where
  Branch :: T n a -> Tree (Succ n) a
  Leaf :: Tree Zero a

data T n a = NodeR (Tree n a) a (Tree (Succ n) a) -- right subtree has height + 1
           | NodeL (Tree (Succ n) a) a (Tree n a) -- left subtree has height + 1
           | Node (Tree n a) a (Tree n a)     -- both subtrees are of equidistant height

type family BoolEq (a :: Nat) (b :: Nat) :: Bool
type instance BoolEq Zero Zero = True
type instance BoolEq (Succ a) (Succ b) = BoolEq a b
type instance BoolEq Zero (Succ x) = False
type instance BoolEq (Succ x) Zero = False



--Example
t :: Tree (Succ Zero) Integer
t = Branch $ NodeR Leaf 3 (Branch (Node Leaf 4 Leaf))
{-
       3
      / \
     .   4
        / \
       .   .

-}

{- Doesn't compile
an unbalanced tree like this

       3
      / \
     .   4
        / \
       .   5
          / \
         .   .

Neither of this compile

x :: Tree (Succ Zero) Integer
x = Branch $ Node Leaf 5 Leaf


t2 :: Tree (Succ (Succ Zero)) Integer
t2 = Branch (NodeR Leaf 3 (Branch (Node Leaf 4 x)))

or

t2 :: Tree (Succ Zero) Integer
t2 = Branch (NodeR Leaf 3 (Branch (Node Leaf 4 x)))
-}
