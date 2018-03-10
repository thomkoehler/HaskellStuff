module Main where

<<<<<<< HEAD
import qualified ScottyTest
import qualified TemplatesTest

main :: IO ()
main = TemplatesTest.main
=======
import qualified TypeStuff
import qualified CallByNeed

main :: IO ()
main = do
  _ <- CallByNeed.test1
  return ()
>>>>>>> c4a5049ed54d970e53cf4e39943a6a05eaac7887
