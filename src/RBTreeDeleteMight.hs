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

reddenTree :: Tree a -> Tree a
reddenTree (T c l x r) = T (lighten c) l x r

maxTree :: Tree a -> a
maxTree (T _ _ x E) = x
maxTree (T _ _ x r) = maxTree r

delete :: (Ord a) => a -> Tree a -> Tree a
delete x s = blackenRoot (del x s)

del x E = E
del x s@(T color a y b)
  | x < y     = bubble color (del x a) y b
  | x > y     = bubble color a y (del x b)
  | otherwise = remove s


remove :: Tree a -> Tree a
remove (T R E _ E) = E
remove (T B E _ E) = E
remove (T B E _ (T R a x b)) = T B a x b
remove (T B (T R a x b) _ E) = T B a x b
remove (T color l y r) = bubble color l' mx r
 where mx = maxTree l
       l' = removeMax l

removeMax :: Tree a -> Tree a
removeMax s@(T _ _ _ E) = remove s
removeMax s@(T color l x r) = bubble color l x (removeMax r)

isBB :: Tree a -> Bool
isBB (T BB _ _ _) = True
isBB _ = False

bubble :: Color -> Tree a -> a -> Tree a -> Tree a
bubble color l x r
 | isBB(l) || isBB(r) = balance (blacken color) (reddenTree l) x (reddenTree r)
 | otherwise          = balance color l x r

balance :: Color -> Tree a -> a -> Tree a -> Tree a
-- Okasaki's insertion cases
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)

-- Matt Might's deletion cases
balance BB (T R (T R a x b) y c) z d = T B (T B a x b) y (T B c z d)
balance BB (T R a x (T R b y c)) z d = T B (T B a x b) y (T B c z d)
balance BB a x (T R (T R b y c) z d) = T B (T B a x b) y (T B c z d)
balance BB a x (T R b y (T R c z d)) = T B (T B a x b) y (T B c z d)

-- Matt Might's negative black cases
balance BB a x (T NB (T B b y c) z (T B d w e))
    = T B (T B a x b) y (balance B c z (T R d w e))
balance BB (T NB (T B a' w b') x (T B b y c)) z d
    = T B (balance B (T R a' w b') x b) y (T B c z d)

-- General cases of balancing
balance color a x b = T color a x b


