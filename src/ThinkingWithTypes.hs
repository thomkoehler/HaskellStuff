{-# LANGUAGE DataKinds #-}

module ThinkingWithTypes(test) where

import GHC.TypeLits

test :: IO ()
test = putStrLn "Hello ThinkingWithTypes"

-- capture 1

-- Curry Howard a^b * a^c = a^(b+c)
exercise141To :: (b -> a, c -> a) -> (Either b c -> a)
exercise141To (f, _) (Left b) = f b
exercise141To (_, g) (Right c) = g c

exercise141From :: (Either b c -> a) -> (b -> a, c -> a)
exercise141From f = (f . Left, f . Right)

-- Curry Howard (a*b)^c = a^c * b^c
exercise142To :: (c -> (a, b)) -> (c -> a, c -> b)
exercise142To f = (fst . f, snd . f)

exercise142From :: (c -> a, c -> b) -> (c -> (a, b))
exercise142From (f, g) c = (f c, g c)

-- Curry Howard (a^b)^c = a^(b*c)
exercise143To :: (c -> b -> a) -> (c, b) -> a
exercise143To = uncurry

exercise143From :: ((c, b) -> a) -> (c -> b -> a) 
exercise143From = curry


-- capture 2
-- CONSTRAINT is the kind of any fully-saturated typeclass.

-- Show Int :: Constraint

-- execirce 2.1.3-i 
-- Show  :: * -> Constraint
-- Functor :: (* -> *) -> Constraint
-- Monad :: (* -> *) -> Constraint
-- MonadTrans :: ((* -> *) -> * -> *) -> Constraint


-- capture 3

data Proxy a = Proxy

-- data UserType
--     = TUser
--     | TAdmin
--
-- data User = User
--     {
--         userAdminToken :: Maybe (Proxy 'TAdmin)
--     }


type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'True y = 'True
    Or 'False y = y
