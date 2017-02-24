{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module BookModels where

import Control.Lens hiding((.=))
import GHC.Generics
import Data.Aeson


data User = User
   {
      _name :: String,
      _eMail :: String,
      _password :: String
   }
   deriving Show


instance FromJSON User where
   parseJSON (Object v) = User <$>
      v .: "name" <*>
      v .: "eMail" <*>
      v .: "password"


instance ToJSON User where
   toJSON (User n em pw) = object ["name" .= n, "eMail" .= em, "password" .= pw]

makeLenses ''User
