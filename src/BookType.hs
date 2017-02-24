------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module BookType(BookType(..)) where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson

------------------------------------------------------------------------------------------------------------------------

data BookType = PaperBook | EBook
   deriving(Show, Read, Eq, Generic)

derivePersistField "BookType"


instance ToJSON BookType where
   toEncoding = genericToEncoding defaultOptions

instance FromJSON BookType

------------------------------------------------------------------------------------------------------------------------
