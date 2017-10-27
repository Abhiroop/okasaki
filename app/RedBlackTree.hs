module RedBlackTree
  (
    Set,
    empty,
    member,
    insert)
where

data Color = R | B

data Tree a = E | T Color (Tree a) a (Tree a)

-- Invariants
-- 1. No red node has a red parent
-- 2. Every path from the root node to an empty node contains the same number of black nodes
-- 3. The root and leaves of the tree are black

-- | Simple Set operations
type Set a = Tree a

empty :: Set a
empty = E

member :: (Ord a) => a -> Set a -> Bool
member x E    = False
member x (T _ a y b)
  | x < y     = member x a
  | x == y    = True
  | otherwise = member x b

-- | Insertion
insert :: (Ord a) => a -> Set a -> Set a
insert x s = makeBlack $ ins s
  where ins E  = T R E x E
        ins (T color a y b)
          | x < y  = balance color (ins a) y b
          | x == y = T color a y b
          | x > y  = balance color a y (ins b)
        makeBlack (T _ a y b) = T B a y b

balance :: Color -> Set a -> a -> Set a -> Set a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b

-- This is simple helper to reduce typing
balance' :: Set a -> Set a
balance' (T color left value right) = balance color left value right

-- | Deletion
delete :: a -> Set a -> Set a
delete x t = makeBlack $ del x t
  where makeBlack (T _ a x b) = T B a x b

-- Delete with consecutive red nodes at the top which is rectified in delete
del :: a -> Set a -> Set a
del = undefined

delL :: a -> Set a -> Set a
delL = undefined

balL :: Set a -> Set a
balL (T B (T R t1 x t2) y t3) = T R (T B t1 x t2) y t3
balL (T B t1@(T B _ _ _) y (T B t2 z t3)) = (T B t1 y (balance'(T R t2 z t3)))
balL (T B t1 y (T R (T B t2 u t3) z t4@(T B l value r))) =
  T R (T B t1 y t2) u (T B t3 z (balance' (T R l value r)))

delR :: Set a -> Set a
delR = undefined

fuse :: Set a -> Set a
fuse = undefined
