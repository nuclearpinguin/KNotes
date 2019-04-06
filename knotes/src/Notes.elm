module Notes            exposing (Book, Note, getId, getTitle 
                        , getAuthor, getBody, getInfo, getBookTitle
                        , getBook, noteDecoder, bookDecoder, initNotes
                        , filterBooks)

import Json.Decode      as D exposing ( Decoder, field, string
                        , list, int, map2, map4 )
import List             exposing (..)


type alias Book = 
  { author : String
  , title : String}


type alias Note = 
  { book: Book
  , body: String
  , info: String
  , id: Int
  }


getAuthor : Note -> String
getAuthor nt = nt.book.author


getTitle : Note -> String
getTitle nt = nt.book.title


getBody : Note -> String
getBody nt = nt.body


getInfo : Note -> String
getInfo nt = nt.info


getId : Note -> Int
getId nt = nt.id

getBook : Note -> Book
getBook nt = nt.book


getBookTitle : Book -> String
getBookTitle bk = bk.title


filterBooks : Book -> List Note -> List Note
filterBooks bk nts = filter (\n -> n.book == bk) nts


initNotes : List Book -> List Note -> List Note
initNotes bks nts = 
  case List.head bks of
    Nothing -> []
    Just bk -> filterBooks bk nts


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

