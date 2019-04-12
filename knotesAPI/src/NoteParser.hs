{-# LANGUAGE OverloadedStrings          #-}

module NoteParser where

import Debug.Trace as Tr
import System.IO

import Text.ParserCombinators.Parsec.Error (messageString, errorMessages)
import Text.Parsec  as P
import Data.Maybe   ( catMaybes )
import Prelude      hiding ( lines, unlines ) -- because we use those from Text
import Data.Text    as T ( Text, dropAround, replace, length, pack, strip )

import Models      as M


eol = char '\n' <|> char '\r' <|> (char '\r' >> return '\n')

-- Parse note header to obtain book title and author
titleAndAuthor :: Parsec Text () (Title, Author)
titleAndAuthor = do
    title <- P.many $ alphaNum <|> noneOf "\n("
    P.char '('
    author <- P.many $ alphaNum <|> noneOf "\n)"
    P.string ")\n"
    return (pack title, pack author)


onlyTitle :: Parsec Text () (Title, Author)
onlyTitle = do
    title <- P.many (alphaNum <|> noneOf "\n")
    P.newline
    return (pack title, "")


noteParser :: Parsec Text () (Title, Author, Info, Body)
noteParser = do
    (title, author) <- (P.try titleAndAuthor) <|> onlyTitle
    P.string "- " 
    info <- P.many (alphaNum <|> noneOf "\n")
    eol >> eol
    body <- P.many (alphaNum <|> noneOf "\n")
    eol
    return (title, author, pack info, pack body)


clippingsParser :: P.Parsec Text () [(Title, Author, Info, Body)]
clippingsParser = P.many $ do
    pair <- noteParser
    P.string "=========="
    eol
    return pair


textToNote :: (Title, Author, Info, Body) -> Maybe M.Note
textToNote ("How to use Knotes", _, _, _) = Nothing
textToNote (title, author, info, body) = Just $ M.createNote book body info
    where
        book = M.createBook (clearAuthors author) (clearTitle title)


makeNotes :: Text -> Either Text [M.Note]
makeNotes txt =
    case parsed of
        Left err -> Left $ pack $ messageString $ head $ errorMessages err
        Right nts -> Right $ catMaybes $ map textToNote nts
    where
        parsed = parse clippingsParser "" (replace "\r" "" txt)

-- Removes unnecessary literals
clearAuthors :: Author -> Author
clearAuthors ts = out
    where
    xs = dropAround (\ch  -> ch `elem` ['[',']', '(',')', '\n']) ts 
    out = strip $ replace "_" " " xs
    

clearTitle :: Title -> Title
clearTitle ts = out
    where
    xs = dropAround (\ch  -> ch `elem` ['[',']','\n']) ts 
    out = strip $ replace "_" " " xs

