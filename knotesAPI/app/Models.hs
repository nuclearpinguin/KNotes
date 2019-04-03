{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Int
import Data.Text    ( Text, pack, splitOn )

import Database.Persist.Class
import Database.Persist.Sqlite 
import Database.Persist.TH


share 
    [ mkPersist sqlSettings
    , mkMigrate "migrateAll"] 
    [persistLowerCase|
        Book json -- The json keyword generates sensible ToJSON and FromJSON instances
            author Text
            title Text
            -- BookKey author title
            deriving Show Eq
        Note json 
            book Book
            body Text
            info Text
            -- NoteKey body
            deriving Show Eq
|]


getBook :: Note -> Book
getBook (Note bk _ _ ) = bk


getAuthor :: Book -> Text
getAuthor (Book a _) = a


getTitle :: Book -> Text
getTitle (Book _ t) = t


makeNoteKey :: Int64 -> Key Note
makeNoteKey n = toSqlKey n 
