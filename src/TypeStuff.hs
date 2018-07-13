
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module TypeStuff where

data LockState 
  = Unlocked
  | Locked

newtype Lock (s :: LockState) = Lock
  {
    getLockId :: Int
  }

newLock :: IO (Lock 'Unlocked)
newLock = undefined

withLock :: Lock 'Unlocked -> (Lock 'Locked -> IO a) -> IO a
withLock = undefined

type family (x :: Bool) <!!> (y :: Bool) :: Bool where
  'True <!!> y = 'True
  'False <!!> y = y

data (a :: k1) :<< (b :: k2)
infixr 5 :<<

class HasPrintf a where
  type Printf a :: *



test :: IO ()
test = putStrLn "Hello World"
