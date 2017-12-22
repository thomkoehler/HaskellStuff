
{-# LANGUAGE RankNTypes #-}

module CallByNeed(test1) where

import Data.IORef
import Data.Maybe
import Text.Printf

import Expr
import Value
import Parser


type Env = [(String, IORef Thunk)]

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
mkThunk env x body th = do
  th' <- newIORef th
  eval ((x, th') : env) body


eval :: Env -> Expr -> IO Value
eval env ex = case ex of
  EVar n -> do
    let ref = fromMaybe (error (printf "Var %s not found." n)) $ lookup n env
    force ref

  ELam x e -> return $ VClosure (mkThunk env x e)

  EApp a b -> do
    VClosure c <- eval env a
    c (\() -> eval env b)

  EBool b -> return $ VBool b
  EInt n  -> return $ VInt n
  EFix e  -> eval env (EApp e (EFix e))
  EPrim primOp expr0 expr1 -> do
    v0 <- eval env expr0
    v1 <- eval env expr1
    case primOp of
      Add -> return $ VInt $ toInt v0 + toInt v1
      Mul -> return $ VInt $ toInt v0 * toInt v1



omega :: Expr
omega = EApp (ELam "x" (EApp (EVar "x") (EVar "x")))
             (ELam "x" (EApp (EVar "x") (EVar "x")))

test1 :: IO ()
test1 = do
  let expr = parseExpr "(\\x -> (\\y -> x + y)) 3 4"
  res <- eval [] expr
  -- res <- eval [] $ EApp (ELam "y" (EInt 42)) omega
  print res
