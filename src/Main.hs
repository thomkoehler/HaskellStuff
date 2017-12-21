module Main where

import qualified TypeStuff
import qualified CallByNeed

main :: IO ()
main = do
  _ <- CallByNeed.test1
  return ()
