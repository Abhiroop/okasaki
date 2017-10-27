module RBTreeDeleteMight where

data Color = R | B | BB | NB

data Tree a = E | T Color (Tree a) a (Tree a)

blacken :: Color -> Color
blacken B  = BB
blacken R  = B
blacken NB = R
blacken BB = error "Can't blacken further"

lighten :: Color -> Color
lighten BB = B
lighten B = R
lighten R = NB
lighten NB = error "Can't lighten further"

blackenRoot :: Tree a -> Tree a
blackenRoot (T _ a x b) = T B a x b



delete :: (Ord a) => a -> Tree a -> Tree a
delete x s = blackenRoot (del x s)

del x E = E
del x s@(T color a y b)
  | x < y     = bubble color (del x a) y b
  | x > y     = bubble color a y (del x b)
  | otherwise = remove s


remove = undefined

bubble = undefined
