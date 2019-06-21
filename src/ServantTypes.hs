{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}


module ServantTypes where

import Data.List

newtype Static = Static String

data Capture = Capture

data Method = Get | Post 

newtype Verb = Verb Method

infixr 5 :>
data a :> b = a :> b

class Endpoint a
instance Endpoint Verb
instance Endpoint rest => Endpoint (Static :> rest)
instance Endpoint rest => Endpoint (Capture :> rest)

endpoint1 = Static "hello" :> Verb Get

type Link = [String]

renderLink :: Link -> String
renderLink xs = '/' : intercalate "/" xs

class HasLink endpoint where
  link :: endpoint -> [String]

instance HasLink api => HasLink (Static :> api) where
  link (Static s :> api) = s : link api

instance HasLink Verb where
  link _ = []
