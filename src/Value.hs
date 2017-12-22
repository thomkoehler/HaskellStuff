
module Value where

data Value
   = VInt Int
   | VBool Bool
   | VClosure (Thunk -> IO Value)
 
type Thunk = () -> IO Value
   
instance Show Value where
   show (VInt v) = "VInt " ++ show v
   show (VBool b) = "VBool " ++  show b
   show (VClosure _) = "VClosure"