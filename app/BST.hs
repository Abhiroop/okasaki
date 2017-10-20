module BST where

type Key = Int -- for experimentation purposes only

data BST = Nil | Node Key BST BST deriving Show



key :: BST -> Key
key Nil = undefined
key (Node a _ _) = a

left :: BST -> BST
left Nil = Nil
left (Node _ l _) = l

right :: BST -> BST
right Nil = Nil
right (Node _ _ r) = r

-- | Search returning boolean
search :: BST -> Key -> Bool
search Nil _ = False
search (Node a l r) k
  | a == k    = True
  | a < k     = search r k
  | otherwise = search l k

-- | Search returning the tree
searchT :: BST -> Key -> BST
searchT Nil _ = Nil
searchT x@(Node a l r) k
  | a == k    = x
  | a < k     = searchT r k
  | otherwise = searchT l k


-- | In order traversal
inorder :: BST -> [Key]
inorder Nil = []
inorder (Node a l r) = (inorder l) ++ [a] ++ (inorder r)

-- | Preorder traversal
preorder :: BST -> [Key]
preorder Nil          = []
preorder (Node a l r) = [a] ++ (preorder l) ++ (preorder r)

-- | Postorder traversal
postorder :: BST -> [Key]
postorder Nil          = []
postorder (Node a l r) = (postorder l) ++ (postorder r) ++ [a]

-- | Minimum of a BST
treeMinimum :: BST -> Key
treeMinimum Nil            = undefined
treeMinimum (Node a Nil _) = a
treeMinimum (Node _ l _)   = treeMinimum l

-- | Maximum of a BST
treeMaximum :: BST -> Key
treeMaximum Nil             = undefined
treeMaximum (Node a _ Nil ) = a
treeMaximum (Node _ _ r)    = treeMaximum r

-- | Find predecessor of an element

type Parent = BstP
type LChild = BstP
type RChild = BstP

data BstP = None | Node1 Key Parent LChild RChild deriving Show

predecessor :: BstP -> Key
predecessor None               = undefined
predecessor (Node1 a p None _) = findPredAncestor a p
predecessor (Node1 a _ l _)    = treeMaximumP l

treeMaximumP :: BstP -> Key
treeMaximumP  None               = undefined
treeMaximumP (Node1 a _ _ None ) = a
treeMaximumP (Node1 _ _ _ r)     = treeMaximumP r

findPredAncestor :: Key -> BstP -> Key
findPredAncestor k (Node1 a p l r)
  | (keyP r) == k = a
  | otherwise = findPredAncestor a p

keyP :: BstP -> Key
keyP  None           = undefined
keyP (Node1 a _ _ _) = a

-- | Find successor of an element

-- | Insertion
insert :: Key -> BST -> BST
insert k Nil = (Node k Nil Nil)
insert k (Node a l r)
  | (k == a)  = (Node a l r)
  | k < a     = insert k l
  | otherwise = insert k r

-- | Deletion

-- | Merging

-- | Balancing

{-
     10
    /   \
  8      12
 / \    /  \
7   9  11   13
-}

x :: BST
x = (Node 10
     (Node 8
       (Node 7 Nil Nil)
       (Node 9 Nil Nil))
     (Node 12
       (Node 11 Nil Nil)
       (Node 13 Nil Nil)))
