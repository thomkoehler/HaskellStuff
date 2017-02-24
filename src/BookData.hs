
{-# LANGUAGE OverloadedStrings #-}

module BookData(allUsers) where

import BookModels


allUsers :: [User]
allUsers =
   [
      User "Alice" "alica@mail.org" "alice",
      User "Bob" "bob@mail.org" "bob"
   ]
