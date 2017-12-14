
module CallByNeed where

import Data.IORef
import Data.Maybe
import Text.Printf


type Name = String

data Value
  = VInt Integer
  | VBool Bool
  | VClosure (Thunk -> IO Value)


data Expr
  = EVar Name
  | ELam Name Expr
  | EApp Expr Expr
  | EInt Int
  | EBool Bool
  | EPrim PrimOp Expr Expr
  | EFix Expr
  deriving(Show)


data PrimOp
  = Add
  | Mul
  deriving(Show)


type Thunk = () -> IO Value


type Env = [(String, Value)]


update :: IORef Thunk -> Value -> IO ()
update ref v = do
  writeIORef ref (\() -> return v)
  return ()


force :: IORef Thunk -> IO Value
force ref = do
  th <- readIORef ref
  v <- th ()
  update ref v
  return v


mkThunk :: Env -> String -> Expr -> (Thunk -> IO Value)
mkThunk env x body = \th -> do
  th' <- newIORef th
  eval ((x, th') : env) body


eval :: Env -> Expr -> IO Value
eval env ex = case ex of
  EVar n -> do
    th <- fromMaybe (error (printf "Var %s not found." n)) $ lookup n env
    v <- force th
    return v

  ELam x e -> return $ VClosure (mkThunk env x e)

  EApp a b -> do
    VClosure c <- eval env a
    c (\() -> eval env b)

  EBool b -> return $ VBool b
  EInt n  -> return $ VInt n
  EFix e  -> eval env (EApp e (EFix e))


omega :: Expr
omega = EApp (ELam "x" (EApp (EVar "x") (EVar "x")))
             (ELam "x" (EApp (EVar "x") (EVar "x")))

test1 :: IO Value
test1 = eval [] $ EApp (ELam "y" (EInt 42)) omega
