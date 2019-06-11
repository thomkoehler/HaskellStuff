
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module DepTypes(test) where

import Prelude hiding(head, tail, replicate)

data Nat 
  = Z
  | S Nat

infixl 7 :*

type family (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z :+ m = m
type instance S n :+ m = S (n :+ m)

type family (n :: Nat) :* (m :: Nat) :: Nat
type instance Z :* m = Z
type instance S n :* m = n :* m :+ m

infixr 5 :-

data Vector a n where
  Nil :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)

deriving instance Eq a => Eq (Vector a n)
deriving instance Show a => Show (Vector a n)

toList :: Vector a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

head :: Vector a (S n) -> a
head (x :- _) = x 

tail :: Vector a (S n) -> Vector a n
tail (_ :- xs) = xs 

append :: Vector a n -> Vector a m -> Vector a (n :+ m)
append Nil v = v
append (x :- xs) v = x :- append xs v

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

infixl 6 :+

(%:+) :: SNat n -> SNat m -> SNat (n :+ m)
SZ %:+ m = m
SS n %:+ m = SS (n %:+ m)


replicate :: SNat n -> a -> Vector a n
replicate SZ _ = Nil
replicate (SS n) x = x :- replicate n x

fromList :: SNat n -> [a] -> Vector a n
fromList SZ _ = Nil
fromList (SS n) (x:xs) = x :- fromList n xs

zipWithSame :: (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n
zipWithSame _ Nil Nil = Nil
zipWithSame fun (x :- xs) (y :- ys) = fun x y :- zipWithSame fun xs ys

data SBool b where
  STrue  :: SBool True
  SFalse :: SBool False

data BTree a
  = Node a
  | BTree (BTree a) (BTree a)



test :: IO ()
test = do
  print $ head (1 :- 2 :- Nil)
  print $ tail (1 :- 2 :- Nil)
  print $ append (1 :- 2 :- Nil) (3 :- 4 :- Nil)
  print $ replicate (SS (SS (SS (SS SZ)))) 4
  print $ fromList (SS (SS (SS (SS SZ)))) [1, 2, 3,4]
  -- type error print $ head Nil
  return ()
