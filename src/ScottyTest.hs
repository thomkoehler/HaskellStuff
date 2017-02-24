   {-# LANGUAGE OverloadedStrings #-}

module ScottyTest(main) where

import Web.Scotty
import Data.Monoid (mconcat)
import Network.Wai.Middleware.HttpAuth
import Data.SecureMem

import BookData
import BookModels

pwd :: SecureMem
pwd = secureMemFromByteString "password"

main :: IO ()
main = scotty 3000 $ do
  middleware $ basicAuth (\u p -> return $ u == "user" && secureMemFromByteString p == pwd) "HaskellStuff"
  get "/users" $ json allUsers
