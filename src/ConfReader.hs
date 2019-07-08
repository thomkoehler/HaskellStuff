{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE GADTs   #-}

module ConfReader where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Binary.Builder
import           Data.HashMap.Strict
import           Data.Text
import qualified Data.Text           as T
import           GHC.Generics        as G
import qualified Text.Megaparsec as Megaparsec
import Data.Void

data Config = Config
    { 
      intEntry :: Int,
      stringEntry :: String,
      boolEntry :: Bool
    }
  deriving (Show, Generic)

instance ToJSON Config where
  toEncoding = genericToEncoding defaultOptions

printValue :: Encoding -> String
printValue value =
  let builder = fromEncoding value
   in show $ toLazyByteString builder

test :: IO ()
test = do
  let conf = Config 1 "Hello" False
      repConf = G.from conf
      jsonConf = genericToJSON defaultOptions conf
      genBool = G.from True
  print $ toConfig conf

toConfig config =
  case genericToJSON defaultOptions config of
    (Object valueMap) ->
      let values = toList valueMap
          lines = Prelude.map valueToConfEntry values
       in T.unlines lines
    _ -> error "Only objects supported"

valueToText :: Value -> T.Text
valueToText (String text)   = text
valueToText (Number number) = T.pack $ show number
valueToText (Bool bool)     = T.pack $ show bool
valueToText (Object _)      = error "Object value is not supported"
valueToText (Array _)       = error "Array value is not supported"
valueToText Null            = error "Null value is not supported"

valueToConfEntry :: (T.Text, Value) -> T.Text
valueToConfEntry (name, value) = T.concat [name, " = ", valueToText value]


type Parser = Megaparsec.Parsec Void T.Text

data ConfigOption c = forall a. ConfigOption
    {
        reader :: ConfReader.Parser a,
        readerLens :: Lens' c a
    }

changeConfig :: T.Text -> c -> ConfigOption c -> c
changeConfig text config (ConfigOption oReader oLens) = 
    case ethValue of
      Left err -> error "Error"
      Right value -> set oLens value config
    where
        ethValue = Megaparsec.parse oReader "" text