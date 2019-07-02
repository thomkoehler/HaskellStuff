
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module RedBlackTree where

data Nat
  = One
  | Succ Nat

data RedBlack
  = Black
  | Red deriving
  (Eq, Ord, Show)


data RedBlackTree a = forall n. T ( Node 'Black n a )

deriving instance Show a => Show (RedBlackTree a)


-- all paths from a node to a leaf have exactly n black nodes
data Node :: RedBlack -> Nat -> * -> * where

  -- all leafs are black
  Leaf :: Node 'Black 'One a

  -- internal black nodes can have children of either color
  B :: Node cL n a -> a -> Node cR n a -> Node 'Black ('Succ n) a

  -- internal red nodes can only have black children
  R :: Node 'Black n a -> a -> Node 'Black n a -> Node 'Red n a
  
deriving instance Show a => Show (Node c n a)


leaf = Leaf 
redNode = R leaf "RED" leaf 
blackNode = B redNode "BLACK" leaf

-- Couldn't match type 'Red with 'Black
-- redNodeError = R redNode "ERROR" blackNode