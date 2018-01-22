module BST where

data BST a = Nil | Node a (BST a) (BST a) deriving (Show, Eq)

key :: BST a -> a
key Nil = undefined
key (Node a _ _) = a

left :: BST a -> BST a
left Nil = Nil
left (Node _ l _) = l

right :: BST a -> BST a
right Nil = Nil
right (Node _ _ r) = r

-- | Search returning boolean
search :: (Ord a) => BST a -> a -> Bool
search Nil _ = False
search (Node a l r) k
  | a == k    = True
  | a < k     = search r k
  | otherwise = search l k

-- | Search returning the tree
searchT :: (Ord a) => BST a -> a -> BST a
searchT Nil _ = Nil
searchT x@(Node a l r) k
  | a == k    = x
  | a < k     = searchT r k
  | otherwise = searchT l k


-- | In order traversal
inorder :: BST a -> [a]
inorder Nil = []
inorder (Node a l r) = (inorder l) ++ [a] ++ (inorder r)

-- | Preorder traversal
preorder :: BST a -> [a]
preorder Nil          = []
preorder (Node a l r) = [a] ++ (preorder l) ++ (preorder r)

-- | Postorder traversal
postorder :: BST a -> [a]
postorder Nil          = []
postorder (Node a l r) = (postorder l) ++ (postorder r) ++ [a]

-- | Minimum of a BST a
treeMinimum :: BST a -> a
treeMinimum Nil            = undefined
treeMinimum (Node a Nil _) = a
treeMinimum (Node _ l _)   = treeMinimum l

-- | Maximum of a BST a
treeMaximum :: BST a -> a
treeMaximum Nil             = undefined
treeMaximum (Node a _ Nil ) = a
treeMaximum (Node _ _ r)    = treeMaximum r

-- | Find predecessor of an element

type Parent = BstP
type LChild = BstP
type RChild = BstP

data BstP a = None | Node1 a (Parent a) (LChild a) (RChild a) deriving Show

predecessor ::(Ord a) => BstP a -> a
predecessor None               = undefined
predecessor (Node1 a p None _) = findPredAncestor a p
predecessor (Node1 a _ l _)    = treeMaximumP l

treeMaximumP :: BstP a -> a
treeMaximumP  None               = undefined
treeMaximumP (Node1 a _ _ None ) = a
treeMaximumP (Node1 _ _ _ r)     = treeMaximumP r

findPredAncestor :: (Ord a) => a -> BstP a -> a
findPredAncestor k (Node1 a p l r)
  | (keyP r) == k = a
  | otherwise = findPredAncestor a p

keyP :: BstP a -> a
keyP  None           = undefined
keyP (Node1 a _ _ _) = a

-- | Find successor of an element

-- | Insertion
insert :: Ord a => a -> BST a -> BST a
insert k Nil = (Node k Nil Nil)
insert k (Node a l r)
  | (k == a)  = (Node a l r)
  | k < a     = insert k l
  | otherwise = insert k r

-- | Deletion
delete :: Ord a => a -> BST a -> BST a
delete k Nil = Nil
delete k x@(Node a l r)
  | (k < a) = delete k l
  | (k > a) = delete k r
  | (k == a) = delete' k x

delete' :: (Ord a) => a -> BST a -> BST a
delete' k (Node a l r)
  | (l == Nil)  = r
  | (r == Nil)  = l
  | otherwise    = let (k,t) = maxAndDelete l
                    in Node k t r

-- This function finds the maximum and then deletes the node as well
maxAndDelete :: (Ord a) => BST a -> (a,BST a)
maxAndDelete t = let m = treeMaximum t
                  in (m,delete m t)

-- | Merging
merge :: (Ord a) => BST a -> BST a -> BST a
merge Nil Nil = Nil
merge a Nil = a
merge Nil a = a
merge a b = let finalList = (inorder a) ++ (inorder b)
             in foldr insert Nil finalList

-- | Balancing

{-
     10
    /   \
  8      12
 / \    /  \
7   9  11   13
-}

x :: BST Integer
x = (Node 10
     (Node 8
       (Node 7 Nil Nil)
       (Node 9 Nil Nil))
     (Node 12
       (Node 11 Nil Nil)
       (Node 13 Nil Nil)))

y = Node 3 (Node 1 Nil Nil) (Node 2 Nil Nil)
