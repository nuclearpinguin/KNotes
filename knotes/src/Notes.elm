module Notes            exposing (Book, Note, Author, Title, getId, getTitle 
                        , getAuthor, getBody, getInfo, getBookTitle
                        , getBook, noteDecoder, bookDecoder, initNotes
                        , filterNotes, getBookAuthor, newBook
                        , bookEncoder, similarNotes)

import Json.Decode      as D exposing ( Decoder, field, string
                        , list, int, map2, map4 )

import Json.Encode      as E exposing ( Value )                        
import List             exposing (..)


type alias Author = String
type alias Title = String

type alias Book = 
  { author : Author
  , title : Title}


type alias Note = 
  { book: Book
  , body: String
  , info: String
  , id: Int
  }

type alias Location =
  { from: Int
  , to: Int
  }

-- HELPERS

getAuthor : Note -> Author
getAuthor nt = nt.book.author


getTitle : Note -> Title
getTitle nt = nt.book.title


getBody : Note -> String
getBody nt = nt.body


getInfo : Note -> String
getInfo nt = nt.info


getId : Note -> Int
getId nt = nt.id

getBook : Note -> Book
getBook nt = nt.book


getBookTitle : Book -> Title
getBookTitle bk = bk.title


getBookAuthor : Book -> Author
getBookAuthor bk = bk.author

newBook : Title -> Author -> Book
newBook t a = {title = t, author = a}


filterNotes : Book -> List Note -> List Note
filterNotes bk nts = filter (\n -> n.book == bk) nts


initNotes : List Book -> List Note -> (Maybe Book, List Note)
initNotes bks nts = 
  case List.head (sortBy .title bks) of
    Nothing -> (Nothing, [])
    Just bk -> (Just bk, filterNotes bk nts)

-- DECODERS

bookDecoder : Decoder Book
bookDecoder = 
  D.map2 Book  
    (field "author" string)
    (field "title" string)


noteDecoder : Decoder Note
noteDecoder = 
  D.map4 Note
    (field "book" bookDecoder)
    (field "body" string)
    (field "info" string)
    (field "id" int)


bookEncoder : Book -> Value
bookEncoder book =
    E.object 
        [ ("author", E.string book.author)
        , ("title", E.string book.title ) ]


-- NOTES DEDUPLICATION

locationToRange : Note -> Maybe (Int, Location)
locationToRange note =
  let 
    range = map String.toInt <| String.split "-" <| String.slice 27 34 <| note.info
  in
  case range of
    [Just x, Just y] -> Just (note.id, {from=x, to=y})
    _ -> Nothing
  

similarNotesIds : (Int, Location) -> List (Int, Location) -> List Int
similarNotesIds (i, l) xs =
  xs |> filter (\(j, x) -> ((x.from == l.from) || (x.to == l.to))) |> map Tuple.first


findSimilarNotes : List (Int, Location) -> List Int
findSimilarNotes nts = 
  case (head nts, tail nts) of
      (Just h, Just t) -> (similarNotesIds h t) ++ (findSimilarNotes t)
      _ -> []
          

similarNotes : List Note -> List Int
similarNotes nts =
  nts |> filterMap locationToRange |> findSimilarNotes

  