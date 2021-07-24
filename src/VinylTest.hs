
-- http://hackage.haskell.org/package/vinyl-0.11.0/docs/Data-Vinyl-Tutorial-Overview.html

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module VinylTest where

import Data.Vinyl
import Data.Vinyl.Functor
import Control.Applicative
import Control.Lens hiding (Identity)
import Control.Lens.TH
import Data.Char
import Test.DocTest
import Data.Singletons.TH (genSingletons)
import Data.Maybe

data Fields 
    = Name 
    | Age 
    | Sleeping 
    | Master 
    deriving Show

type LifeForm = [Name, Age, Sleeping]

type family ElF (f :: Fields) :: * where
    ElF Name = String
    ElF Age = Int
    ElF Sleeping = Bool
    ElF Master = Rec Attr LifeForm

newtype Attr f = Attr { _unAttr :: ElF f }

makeLenses ''Attr

genSingletons [ ''Fields ]

instance Show (Attr Name) where show (Attr x) = "name: " ++ show x
instance Show (Attr Age) where show (Attr x) = "age: " ++ show x
instance Show (Attr Sleeping) where show (Attr x) = "sleeping: " ++ show x
instance Show (Attr Master) where show (Attr x) = "master: " ++ show x

(=::) :: sing f -> ElF f -> Attr f
_ =:: x = Attr x

-- wakeUp :: (Sleeping ∈ fields) => Rec Attr fields -> Rec Attr fields
-- wakeUp = rput $ SSleeping =:: False

test :: IO ()
test = do
    let jon = (SName =:: "jon") :& (SAge =:: 23) :& (SSleeping =:: False) :& RNil
    print jon

    let tucker = (SName =:: "tucker") :& (SAge =:: 9) :& (SSleeping =:: True) :& (SMaster =:: jon) :& RNil
    print tucker

