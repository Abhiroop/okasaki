module BST where

type Key = Int -- for experimentation purposes only

data BST = Nil | Node Key BST BST

key :: BST -> Key
key Nil = undefined
key (Node a _ _) = a

left :: BST -> BST
left Nil = Nil
left (Node _ l _) = l

right :: BST -> BST
right Nil = Nil
right (Node _ _ r) = r

-- | Search
search :: BST -> Key -> Bool
search Nil _ = False
search (Node a l r) k
  | a == k    = True
  | a < k     = search r k
  | otherwise = search l k

-- | In order traversal
inorder :: BST -> [Key]
inorder Nil = []
inorder (Node a l r) = (inorder l) ++ [a] ++ (inorder r)

-- | Preorder traversal
preorder :: BST -> [Key]
preorder Nil = []
preorder (Node a l r) = [a] ++ (preorder l) ++ (preorder r)

-- | Postorder traversal
postorder :: BST -> [Key]
postorder Nil = []
postorder (Node a l r) = (postorder l) ++ (postorder r) ++ [a]

-- | Insertion

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
