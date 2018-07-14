{-# LANGUAGE LambdaCase #-}
module Tree
  (
  Tree(..), 
  BinaryTree(..),
  bfsCursor,
  BFScursor(..)
  )
    where


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

class BinaryTree f where 
  valueOf :: f a -> Maybe a
  left :: f a -> f a
  right :: f a -> f a
  children :: f a -> [f a]
  isEmpty :: f a -> Bool

instance BinaryTree Tree where
  valueOf = \case 
    Empty -> Nothing
    (Node v _ _) -> Just v
  left = \case 
   (Node _ l _) -> l
   Empty -> Empty
  right = \case
   (Node _ _ r) -> r
   Empty -> Empty
  children Empty = []
  children t =
    filter (
      \case 
        Empty -> True 
        _ -> False 
     ) [left t, right t]
  isEmpty = \case
    Empty -> False
    _ -> True

data BFScursor a = BFScursor {
  value :: a,
  parent ::  Maybe a,
  nextParents :: [a],
  history :: [BFScursor a],
  nextSiblings :: [a]
} deriving (Show)

bfsCursor :: (BinaryTree f) => f a -> BFScursor (f a)
bfsCursor t
  | isEmpty t = BFScursor t Nothing [] [] []
  | otherwise = BFScursor t Nothing [t] [] []


class Cursor cursor where 
  next :: BinaryTree f => cursor (f a) -> Maybe (cursor (f a))
  previous :: BinaryTree f => cursor (f a) -> Maybe (cursor (f a))

instance Cursor BFScursor where 
  previous cursor = case history cursor of
                      [] -> Nothing
                      (x:_) -> Just x

  next cursor =
    let newHistory = (cursor : history cursor)
     in 
      case nextSiblings cursor of 
        (x : xs) -> Just (cursor {value = x, nextSiblings = xs, history = newHistory})
        [] -> case nextParents cursor of
                    [] -> Nothing
                    (newParent : otherParents) ->
                      case children newParent of 
                        [] -> next ( cursor {nextParents = otherParents})
                        (firstChild : otherChildren) -> Just (BFScursor firstChild (Just newParent) (otherParents ++ children newParent) newHistory otherChildren)

