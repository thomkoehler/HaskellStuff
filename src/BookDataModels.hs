
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


module BookDataModels where

import Database.Persist.TH
import Data.Text
import Data.Time.Calendar
import GHC.Generics
import Data.Aeson

import BookType


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User json
   name String
   eMail String
   password String
   deriving Show

Book json
   isbn Text Maybe
   title Text
   publisher PublisherId Maybe
   bookType BookType default=PaperBook
   deriving Show

Author json
   name Text
   birthDate Day Maybe
   deathDate Day Maybe
   deriving Show

BookAuthor json
   bookId BookId
   authorId AuthorId
   UniqueBookAuthor bookId authorId
   deriving Show

Publisher json
   name Text
   deriving Show
   UniquePublisherName name

|]
