
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module RunQ(printInstance) where

import Language.Haskell.TH.Syntax
import Data.Aeson
import Data.String

data TestModel = TestModel
  {
    first :: Int,
    second :: String
  }
  deriving(Eq, Ord, Show)
  
  
instance ToJSON TestModel where
  toJSON m = object [fromString "first" .= first m, fromString "second" .= second m] 

 
  
printInstance = runQ [d| instance ToJSON TestModel where toJSON m = object [fromString "first" .= first m, fromString "second" .= second m] |] >>= print
  


