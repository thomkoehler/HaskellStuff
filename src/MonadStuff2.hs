module MonadStuff2 where

import Control.Monad.Trans.State
import Control.Monad.Except

data CalcError = StackOverflow
    deriving Show

type CalcM = ExceptT CalcError (State [Double])

push :: Double -> CalcM ()
push x = lift $ modify (x:)

pop :: CalcM Double
pop = do
    st <- lift get
    case st of
        [] -> throwError StackOverflow
        (x:xs) -> lift $ put xs >> return x

binOp :: (Double -> Double -> Double) -> CalcM ()
binOp op = do
    x <- pop
    y <- pop
    push $ x `op` y

add :: CalcM ()
add = binOp (+)

mul :: CalcM ()
mul = binOp (*)

parseCalc :: String -> CalcM ()
parseCalc "pop" = void pop
parseCalc "+" = add
parseCalc "*" = mul
parseCalc dtxt = push $ read dtxt

parse :: String -> CalcM ()
parse txt = mapM_ parseCalc (words txt)

test :: IO ()
test = do
    print test1
    print test2

test1 = runState (runExceptT (parse "1.1 2.2 * 3.3 +")) []

test2 = runState (runExceptT (parse "pop")) []