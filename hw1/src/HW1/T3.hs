{-# LANGUAGE RecordWildCards #-}

module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Meta = Meta {size :: Int, depth :: Int}
  deriving (Show)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                    = 0
tsize (Branch Meta{..} _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                    = 0
tdepth (Branch Meta{..} _ _ _) = depth

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember value (Branch _ left this right)
  | value == this = True
  | value < this = tmember value left
  | otherwise = tmember value right

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert value Leaf = mkBranch Leaf value Leaf
tinsert value node@(Branch _ left this right)
  | value == this = node
  | value < this = balanceTree $ mkBranch (tinsert value left) this right
  | otherwise = balanceTree $ mkBranch left this (tinsert value right)

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left this right = Branch (Meta {size  = 1 + tsize left + tsize right,
                                         depth = 1 + max (tdepth left) (tdepth right)})
                                  left this right

balanceTree :: Tree a -> Tree a
balanceTree Leaf = Leaf
balanceTree self@(Branch _ left _ right)
  | tdepth right - tdepth left == 2 = leftRotate self
  | tdepth left - tdepth right == 2 = rightRotate self
  | otherwise = self

leftRotate :: Tree a -> Tree a
leftRotate self@(Branch _ left aVal (Branch _ center bVal right))
  | tdepth center > tdepth right = leftBigRotate self
  | otherwise = mkBranch (mkBranch left aVal center) bVal right
leftRotate x = x

rightRotate :: Tree a -> Tree a
rightRotate self@(Branch _ (Branch _ left bVal center) aVal right)
  -- When called from xBigRotate tdepth is equal, no endless recursion there
  | tdepth center > tdepth left = rightBigRotate self
  | otherwise = mkBranch left bVal $ mkBranch center aVal right
rightRotate x = x

leftBigRotate :: Tree a -> Tree a
leftBigRotate (Branch _ left a right) = leftRotate $ mkBranch left a $ rightRotate right
leftBigRotate x                       = x

rightBigRotate :: Tree a -> Tree a
rightBigRotate (Branch _ left a right) = rightRotate $ mkBranch (leftRotate left) a right
rightBigRotate x                       = x
