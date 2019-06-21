{-# LANGUAGE TemplateHaskell #-}

module SingletonTest where

import Data.Singletons.TH

$(singletons [d|
    data Nat = Zero | Succ Nat
    pred :: Nat -> Nat
    pred Zero = Zero
    pred (Succ n) = n
|])


test :: IO ()
test = return ()