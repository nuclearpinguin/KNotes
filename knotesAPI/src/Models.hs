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
import Data.List    ( nub )

import Database.Persist.Class
import Database.Persist.Sqlite 
import Database.Persist.TH


type Body = Text
type Info = Text
type Title = Text
type Author = Text


share 
    [ mkPersist sqlSettings
    , mkMigrate "migrateAll"] 
    [persistLowerCase|
        Book json -- The json keyword generates sensible ToJSON and FromJSON instances
            author Author
            title Title
            UniqueBook author title
            deriving Show Eq
        Note json 
            book Book
            body Body
            info Text
            UniqueNote book info
            -- test Tezt default=''
            deriving Show Eq
|]

-- Helpers

createBook :: Author -> Title -> Book
createBook a t = Book a t


createNote :: Book -> Body -> Info -> Note
createNote bk bd info = Note bk bd info


getBook :: Note -> Book
getBook (Note bk _ _ ) = bk


getAuthor :: Book -> Author
getAuthor (Book a _) = a


getTitle :: Book -> Title
getTitle (Book _ t) = t


makeNoteKey :: Int64 -> Key Note
makeNoteKey n = toSqlKey n 


uniqueBooks :: [Note] -> [Book]
uniqueBooks ns = nub $ map getBook ns


uniqueAuthors :: [Book] -> [Author]
uniqueAuthors bs = nub $ map getAuthor bs


uniqueTitles :: [Book] -> [Title]
uniqueTitles bs = nub $ map getTitle bs
