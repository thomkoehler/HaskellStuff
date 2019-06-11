
module FreeMonad where

data Toy next
  = Output String next
  | Bell next
  | Done

newtype Fix f = Fix (f (Fix f))