module FreeMonad where

data Toy b next
    = Output b next
    | Bell next
    | Done
    deriving Show

data FixE f e
    = Fix (f (FixE f e))
    | Throw e

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f   = Fix (fmap (flip catch f) x)
catch (Throw e) f = f e

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell next) = Bell (f next)
    fmap _ Done = Done
