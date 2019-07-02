
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}


module DepTypes where

import Prelude hiding(head, tail, replicate)
import Data.Kind

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


-----------------------------------------------------------------------------------

type family Cond (b :: Bool) t e where
  Cond 'True t e = t
  Cond 'False t e = e

type family a :=: b where
  a :=: a = 'True
  _ :=: _ = 'False

item :: SBool b -> Cond b Int String
item STrue = 1
item SFalse = "Hello"

-----------------------------------------------------------------------------------

data DoorState 
  = Opened
  | Closed
  | Locked
  deriving(Show, Eq)

 
-- data Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String }
data Door :: DoorState -> Type where
  UnsafeMkDoor :: String -> Door s
  deriving Show


closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

data SDoorState :: DoorState -> Type where
  SOpened :: SDoorState 'Opened
  SClosed :: SDoorState 'Closed
  SLocked :: SDoorState 'Locked

lockAnyDoor :: SDoorState s -> Door s -> Door 'Locked
lockAnyDoor = \case
    SOpened -> lockDoor . closeDoor
    SClosed -> lockDoor
    SLocked -> id


doorStatus :: SDoorState s -> Door s -> DoorState
doorStatus SOpened _ = Opened
doorStatus SClosed _ = Closed
doorStatus SLocked _ = Locked

-----------------------------------------------------------------------------------


test :: IO ()
test = do
  print $ head (1 :- 2 :- Nil)
  print $ tail (1 :- 2 :- Nil)
  print $ append (1 :- 2 :- Nil) (3 :- 4 :- Nil)
  print $ replicate (SS (SS (SS (SS SZ)))) 4
  print $ fromList (SS (SS (SS (SS SZ)))) [1, 2, 3,4]
  -- type error print $ head Nil

  return ()
