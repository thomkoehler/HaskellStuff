{-# LANGUAGE TemplateHaskell #-}

module Lenses(test) where

import Control.Lens

data Names = Names 
    {
        _firstName :: String,
        _lastName :: String
    }
    deriving Show

data Person = Person
    {
        _names :: Names,
        _age :: Int
    }
    deriving Show

$(makeLenses ''Names)
$(makeLenses ''Person)


anakin :: Person
anakin = Person (Names "Anakin" "Skywalker") 41

han :: Person
han = Person (Names "Han" "Solo") 34



test :: IO ()
test = do
    print anakin

    -- View (^.)
    print $ anakin ^. names
    print $ anakin ^. names . firstName
    print $ view (names . firstName) anakin

    -- Set (.~)
    let tom = anakin & names . firstName .~ "Tom"
    let tom' = set (names . firstName) "Tom" anakin 
    print tom
    print tom'

    -- Over
    let lea = over (names . firstName) (const "Lea") anakin
    let lea' = anakin & (names . firstName) %~ (const "Lea")
    print lea

    return ()