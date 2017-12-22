
module Value where

type Thunk = () -> IO Value

data Value
   = VInt Int
   | VBool Bool
   | VClosure (Thunk -> IO Value)
  
instance Show Value where
   show (VInt v) = "VInt " ++ show v
   show (VBool b) = "VBool " ++  show b
   show (VClosure _) = "VClosure"


toInt :: Value -> Int
toInt (VInt i) = i
toInt _ = error "VInt expected."