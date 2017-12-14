
{-# LANGUAGE PolyKinds #-}

module TypeStuff(test) where

class TypeStr a where
  typeStr :: a -> String

instance TypeStr Int where
  typeStr _ = "Int"

instance TypeStr (Maybe a) where
  typeStr _ = "Maybe"


test :: IO ()
test = do
  print $ typeStr (1 :: Int)
  print $ typeStr $ Just '1'
  print $ typeStr Nothing
