
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module RunQ(printInstance1, printInstance2) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
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

 
data StrData = StrData String
  
printInstance1 = runQ [d| instance ToJSON TestModel where toJSON m = object [fromString "first" .= first m, fromString "second" .= second m] |] >>= print

printInstance2 = runQ [e| StrData "Hello" |] >>= print
  


