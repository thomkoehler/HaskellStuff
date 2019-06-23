
{-# LANGUAGE DatatypeContexts #-}

module ExpressionProblem where

newtype Lit = Lit Int
data (Exp l, Exp r) => Add l r = Add l r

class Exp x
instance Exp Lit
instance (Exp l, Exp r) => Exp (Add l r)
 
class Exp e => PrettyPrint e where
  prettyPrint :: e -> IO ()

instance PrettyPrint Lit where
  prettyPrint (Lit x) = putStr $ show x

instance (PrettyPrint l, PrettyPrint r) => PrettyPrint (Add l r) where
  prettyPrint (Add l r) = do
    prettyPrint l
    putStr " + "
    prettyPrint r

class Exp e => Evaluate e where
  evaluate :: e -> Int

instance Evaluate Lit where
  evaluate (Lit i) = i

instance (Evaluate l, Evaluate r) => Evaluate (Add l r) where
  evaluate (Add l r) = evaluate l + evaluate r