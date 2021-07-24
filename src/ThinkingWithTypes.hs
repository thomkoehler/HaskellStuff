{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}


module ThinkingWithTypes where

-- import GHC.TypeLits
import Data.Typeable
import Data.Kind(Type)

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


-- data Proxy a = Proxy

-- data UserType
--     = TUser
--     | TAdmin

-- data User = User
--     {
--         userAdminToken :: Maybe (Proxy 'TAdmin)
--     }


type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'True y = 'True
    Or 'False y = y

type family Not (x :: Bool) :: Bool where
    Not 'True = 'False
    Not 'False = 'True

-- capture 3

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
    fmap f (T1 g) = T1 (f . g)


newtype T5 a = T5 ((a -> Int) -> Int)

-- TODO
-- instance Functor T5 where
--     fmap f (T5 g) = T5 (g . f)

-- capture 4.3 Ambiguous Types and Non-Injectivity

typename :: forall a. Typeable a => String
typename = show . typeRep $ Proxy @a

-- capture 5.2 GADTs

data Expr a where
    LitInt :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    If :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a 
evalExpr (LitInt i) = i
evalExpr (LitBool b) = b
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (If b x y) = if evalExpr b then evalExpr x else evalExpr y


data Expr_ a
    = (a ~ Int) => LitInt_ Int
    | (a ~ Bool) => LitBool_ Bool
    | (a ~ Int) =>   Add_ (Expr_ Int) (Expr_ Int)
    | (a ~  Bool) =>   Not_ (Expr_ Bool)
    | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

evalExpr_ :: Expr_ a -> a
evalExpr_ (LitInt_ i) = i

-- exercice 5.3. HETEROGENEOUS LISTS

data HList (ts :: [Type]) where
    HNil :: HList '[]
    (:#) :: t -> HList ts -> HList (t ': ts)

infix 5 :#

hLenght :: HList ts -> Int
hLenght HNil = 0
hLenght (_ :# ts) = 1 + hLenght ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t
