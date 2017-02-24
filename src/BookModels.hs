{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BookModels where

import GHC.Generics
import Data.Aeson
import Data.Int


data Book = Book
   {
      id :: Int64
   }

data User = User
   {
      id :: Int64,
      name :: String,
      eMail :: String,
      password :: String
   }
   deriving Show


instance FromJSON User where
   parseJSON (Object v) = User <$>
      v .: "id" <*>
      v .: "name" <*>
      v .: "eMail" <*>
      v .: "password"


instance ToJSON User where
   toJSON (User i n em pw) = object ["id" .= i, "name" .= n, "eMail" .= em, "password" .= pw]
