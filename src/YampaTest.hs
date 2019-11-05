-- http://tuttlem.github.io/2014/07/31/functional-reactive-programming-with-yampa.html

{-# LANGUAGE Arrows #-}

module YampaTest where

import FRP.Yampa

test = test1


sensorData :: (Float, [(DTime, Maybe Float)])
sensorData = (80.0, [(60.0, Just 74.6), (60.0, Just 68.9), (60.0, Just 61.5)])

test0 :: IO ()
test0 = do
    print $ embed time sensorData
    print $ embed identity sensorData
    print $ embed (constant 77) sensorData


cooling :: Float -> SF () (Float)
cooling t0 = proc input -> do
    t0' <- integral >>^ (+ t0) -< -1
    returnA -< t0'

test1 :: IO ()
test1 = do
    print $ embed (cooling 80.0) (100.0, [])


test2 :: IO ()
test2 = reactimate 
    (return ())
    (\_ -> return (1.0, Nothing))
    (\_ b -> (putStrLn $ show b) >> return False)
    (coolingWithFloor 25.0)

coolingWithFloor :: Double -> SF () (Double)
coolingWithFloor t0 = switch cooling' atRoomTemp
    where 
        cooling' = proc _ -> do
            t' <- cooling t0 -< ()
            e <- edge -< t' <= 18
            returnA -< (t', e `tag` t')

        atRoomTemp _ = (constant 18)

        cooling t0 = (constant (-1) >>> integral) >>^ (+ t0)