{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module ConfReader where

import GHC.Generics as G
import Control.Lens
import Data.Aeson
import Data.Binary.Builder


data ConfigType 
    = Type0 
    | Type1 
    | Type2 
    deriving(Show, Generic)

data Config = Config
    {
        _confType :: ConfigType,
        _entry1 :: Int,
        _entry2 :: String
    }
    deriving(Show, Generic)

makeLenses ''Config

instance ToJSON ConfigType where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions


printValue :: Encoding -> String
printValue value = 
    let 
        builder = fromEncoding value
    in
        show $ toLazyByteString builder 


test :: IO ()
test = do
    let 
        conf = Config Type0 1 "Hello"
        repConf = G.from conf
        jsonConf = genericToEncoding defaultOptions conf
    
    print $ printValue jsonConf
