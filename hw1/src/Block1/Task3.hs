{-# LANGUAGE InstanceSigs #-}

module Block1.Task3 where

-- | Data type for binnary search tree
data Tree a = Leaf
    | Node
    { leftBranch  :: Tree a -- left subtree
    -- Tree knot
    , joint       :: [a] -- Tree knot
    -- right subtree
    , rightBranch :: Tree a -- right subtree
    }
    deriving (Show, Eq)

-- | Function that check whether the "Tree" is empty
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Tree size calculation
getSize :: Tree a -> Int
getSize Leaf                   = 0
getSize (Node left list right) = getSize left + getSize right + length list

-- | Search for the specified item in the tree
find :: Ord a => a -> Tree a -> Bool
find _ Leaf = False
find el (Node left list right)
  | el < head list = find el left
  | el == head list = True
  | otherwise = find el right

-- | Insert a new item into the tree
insert :: Ord a => a -> Tree a -> Tree a
insert el Leaf = Node Leaf [el] Leaf
insert el (Node left list right)
  | el == head list = Node left (el : list) right
  | el < head list = Node (insert el left) list right
  | otherwise = Node left list $ insert el right

-- | Creating a tree from the list of elements
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

-- | Ties two tree into one
bind :: Tree a -> Tree a -> Tree a
bind Leaf Leaf         = Leaf
bind Leaf (Node a b c) = Node a b c
bind (Node a b c) Leaf = Node a b c
bind left (Node a b c) = Node (bind a left) b c

-- | Removing the given element from the tree
delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete el (Node left list right)
  | el == head list =
    if length list == 1
      then bind left right
      else Node left (tail list) right
  | el < head list = Node (delete el left) list right
  | otherwise = Node left list (delete el right)


------------- Block2.Task1 -------------

-- | Creating a list of elements from the tree
toList :: Tree a -> [a]
toList = foldr (:) []

-- | `Foldable` Tree realization
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ acc Leaf = acc
  foldr f acc (Node left list right) = foldr f (foldr f fright list) left
    where
      fright = foldr f acc right

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f = foldr ((<>) . f) mempty
