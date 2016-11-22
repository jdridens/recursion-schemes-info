{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeFamilies #-}

module KmettRecursion where

import Data.Functor.Foldable
import Data.List.Ordered
import Control.Monad.State.Lazy

data BTree a = Leaf a | Node (BTree a) (BTree a) | Empty
  deriving (Functor, Foldable, Traversable, Show)

data BTreeF a b = LeafF a | NodeF b b | EmptyF
  deriving (Functor, Foldable, Traversable)

type instance Base (BTree a) = BTreeF a

instance Recursive (BTree a) where
  project Empty = EmptyF
  project (Leaf a) = LeafF a
  project (Node a b) = NodeF a b

instance Corecursive (BTree a) where
  embed EmptyF = Empty
  embed (LeafF a) = Leaf a
  embed (NodeF a b) = Node a b

foo :: BTree ()
foo = Node (Node (Node (Leaf ()) Empty) Empty) (Leaf ())

build :: [a] -> BTree a
build = ana coalg where
  coalg []  = EmptyF
  coalg [x] = LeafF x
  coalg xs  = NodeF l r
    where (l,r) = splitAt (length xs `div` 2) xs

squash :: BTree a -> [a]
squash = cata alg where
  alg EmptyF = []
  alg (LeafF x) = [x]
  alg (NodeF l r) = l ++ r

depth :: Ord a => BTree a -> Int
depth = cata alg where
    alg EmptyF = 0
    alg (LeafF _) = 1 
    alg (NodeF a b) = 1 + (maximum [a,b])

mergesort :: Ord a => [a] -> [a]
mergesort = hylo alg coalg where
  coalg [] = EmptyF
  coalg [x] = LeafF x
  coalg xs = NodeF l r
    where (l,r) = splitAt (length xs `div` 2) xs
  alg EmptyF = []
  alg (LeafF x) = [x]
  alg (NodeF l r) = merge l r

sliding :: Int -> [a] -> [[a]]
sliding n = para alg where
    alg Nil = []
    alg (Cons x (r, xs)) = [x:take (n-1) r] ++ xs

