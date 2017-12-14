{-# LANGUAGE OverloadedStrings #-}

module ScottyTest(test) where

import Web.Scotty
import Data.Monoid (mconcat)
import Network.Wai.Middleware.HttpAuth
import Data.SecureMem

import BookData
import BookDataModels

pwd :: SecureMem
pwd = secureMemFromByteString "password"

test :: IO ()
test = scotty 3000 $ do
  middleware $ basicAuth (\u p -> return $ u == "user" && secureMemFromByteString p == pwd) "HaskellStuff"
  get "/users" $ json allUsers
