{-# LANGUAGE OverloadedStrings          #-}

module NoteParser   ( readNote, uniqueAuthors
                    , uniqueTitles, uniqueBooks
                    , makeNotes ) where

import qualified Data.ByteString as B
import Data.Text.Encoding 
import Data.Maybe           ( catMaybes )
import Data.List            (nub)
import Prelude hiding       (lines, unlines) 
import Data.Text            ( Text, pack, unpack, splitOn 
                            , lines, unlines, dropAround)

import Models as M

-- Removes unnecessary literals
clearAuthors :: Text -> Text
clearAuthors ts = dropAround (\ch  -> ch `elem` [')', '\n']) ts


-- Creates Book from text
textToBook :: Text -> M.Book
textToBook info = M.Book author (head xs)
    where
        xs = splitOn " (" info
        author = clearAuthors (unlines $ tail xs)

        
-- Converts splitted note's text into a Note
parseNote :: [Text] -> M.Note
parseNote items = M.Note book body info
    where
        book = textToBook $ head items
        info = head $ tail items
        -- 3rd tail to avoid first \n
        body = unlines $ tail $ tail $ tail items 


-- TODO: this maybe unwanted
readNote :: Text -> Maybe M.Note
readNote ts =
    case (head items) of
        "How to use Knotes" -> Nothing
        _ -> Just $ parseNote items
    where
        items = splitOn "\n" ts


-- Converts raw text into notes
makeNotes :: B.ByteString -> [M.Note]
makeNotes = catMaybes . map readNote . splitOn "==========\n" . decodeUtf8


uniqueBooks :: [M.Note] -> [M.Book]
uniqueBooks ns = nub $ map M.getBook ns


uniqueAuthors :: [M.Book] -> [Text]
uniqueAuthors bs = map M.getAuthor bs


uniqueTitles :: [M.Book] -> [Text]
uniqueTitles bs = map M.getTitle bs

