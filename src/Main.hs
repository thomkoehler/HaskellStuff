
module Main where

import Prelude hiding(foldr, foldl)

import qualified RunQ
import qualified DepTypes
import qualified Fun
import qualified VinylTest
import qualified ConfReader
import qualified MonadStuff2
import qualified YampaTest
import qualified Circuit


mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

mySumTR :: Num a => [a] -> a
mySumTR = mySum' 0
    where
        mySum' x [] = x
        mySum' y (x:xs) = mySum' (x + y) xs

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ a [] = a
foldl f a (b:bs) = f (foldl f a bs) b

-- foldl (+) 0 [1, 2]
-- (foldl (+) 0 [2]) + 1
-- (foldl (+) 0 []) + 1 + 2
-- 0 + 1 + 2
   
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b [] = b
foldr f b (a:as) = f a $ foldr f b as

-- foldr (+) 0 [1, 2]
-- 1 + (foldr (+) 0 [2])
-- 1 + 2 + (foldr (+) 0 [])
-- 1 + 2 + 0


main :: IO ()
main = do
    -- print $ mySum [0..9]
    -- print $ mySumTR [0..9]
    -- print $ foldl (+) 0 [0..9]
    -- print $ foldr (+) 0 [0..9]
    YampaTest.test
