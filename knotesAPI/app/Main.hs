{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}


import System.IO
import Web.Spock                    as WS
import Web.Spock.Config
import Data.Aeson                   hiding ( json ) -- Because we use Web.Spock.json
import Data.Monoid                  ( (<>) )
import Network.HTTP.Types           ( Status, ok200, created201, accepted202
                                    , badRequest400, notFound404 )

import Control.Monad.Logger         ( LoggingT, runStdoutLoggingT )
import Database.Persist             hiding ( get ) -- To avoid clash with Web.Spock.get
import Database.Persist.Sqlite      as PSQL hiding ( get )
import Database.Persist.TH          

import Data.Int
import Data.HashMap.Strict          ( (!), keys )
import Data.Text                    ( Text )
import NoteParser                   ( readNote, uniqueTitles, uniqueBooks, makeNotes )
import Models


runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn


-- API  error handlers
errorHandler ::  Status ->  ActionCtxT () IO ()
errorHandler s 
    | s == notFound404 = json $ object
                        [ "result" .= String "failure"
                        , "msg" .= String "Page not found."]
    | otherwise = json $ object
                    [ "result" .= String "failure"
                    , "msg" .= String "Something very bad happened!"]

            
-- SERVER
main :: IO ()
main = do
    -- Create db connection
    pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
    
    -- Add custom error handling
    spockCfg <- (defaultSpockCfg () (PCPool pool) ()) >>= \cfg -> 
        return $ cfg {spc_errorHandler = errorHandler}
    
    -- Migrate
    runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool

    runSpock 3000 (spock spockCfg app)


-- API
type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a

app :: Api
app = do
    get "books" $ do
        allBooks <- runSQL $ selectList [] [Asc BookId]
        json $ allBooks

    post "books" $ do
        maybeBook <- jsonBody :: ApiAction (Maybe Book)
        case maybeBook of
            Nothing -> setStatus badRequest400
            Just book -> do
                newId <- runSQL $ insert book
                setStatus created201 

    get "notes" $ do
        allNotes <- runSQL $ selectList [] [Asc NoteId]
        json $ allNotes

    post "notes" $ do
        maybeNote <- jsonBody :: ApiAction (Maybe Note)
        case maybeNote of
            Nothing -> setStatus badRequest400
            Just note -> do
                newId <- runSQL $ insert note
                setStatus created201 
        
    WS.delete "notes" $ do
        maybeId <- jsonBody :: ApiAction (Maybe Int64)
        case maybeId of
            Nothing -> setStatus badRequest400
            Just id -> do
                rmvId <- runSQL $ PSQL.delete (makeNoteKey id)
                setStatus accepted202 
    
    post "test" $ do
        txt <- body 
        let notes = makeNotes txt
        ids <- mapM (runSQL.insert) notes
        json notes
