-- https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial

{-# LANGUAGE Arrows #-}

module Circuit(test) where

import Control.Arrow
import qualified Control.Category as Cat

newtype Circuit a b = Circuit 
    { 
        unCircuit :: a -> (Circuit a b, b) 
    }

instance Cat.Category Circuit where
    id = Circuit $ \a -> (Cat.id, a)
    (.) = dot
        where
            (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
                let
                    (cir1', b) = cir1 a
                    (cir2', c) = cir2 b
                in
                    (cir2' `dot` cir1', c)

instance Arrow Circuit where
    arr f = Circuit $ \a -> (arr f, f a)
    first (Circuit cir) = Circuit $ \(b, d) -> 
        let
            (cir', c) = cir b
        in
            (first cir', (c, d))

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _ [] = []
runCircuit cir (x:xs) = 
    let
        (cir', x') = unCircuit cir x
    in
        x' : runCircuit cir' xs

accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc fun = Circuit $ \inp ->
    let
        (out, acc') = fun inp acc
    in
        (accum acc' fun, out)

accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = a `f` b in (b', b'))

total :: Num a => Circuit a a
total = accum' 0 (+)

mean1 :: Fractional a => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

mean2 :: Circuit Double Double
mean2 = proc value -> do
    t <- total -< value
    n <- total -< 1
    returnA -< t / n

test :: IO ()
test = do
    print $ runCircuit (arr id) [1, 0, 1, 0, 0, 2]
    print $ runCircuit total [1, 0, 1, 0, 0, 2]
    print $ runCircuit mean1 [1, 2, 3]
    print $ runCircuit mean2 [1, 2, 3]