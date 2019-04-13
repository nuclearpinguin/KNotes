module Main             exposing ( .. )

import Debug            exposing ( log, toString )
import List             exposing  ( .. )
import List.Extra       exposing ( uniqueBy )

import Browser
import Css              exposing (..)
import Html.Styled      as Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events     as Event 

-- Files,http and encode / decode
import Json.Encode      as E
import Json.Decode      as D exposing (Decoder, string, list, int, map2, map4 )
import Http             exposing ( Error )
import Url.Builder      as Builder
import File.Select      as Select
import File             exposing (File)
import Task

-- Custom imports
import Style            as S
import Notes            exposing (..)


main =
  Browser.element 
    { init = init
    , update = update
    , view = view >> toUnstyled
    , subscriptions = subscriptions
    }

-- MODEL

type alias Load = 
  { notes: List Note
  , selectedNotes: List Note
  , books: List Book
  , selectedBook: Book
  }


type Model = New 
  | Failure Error 
  | Loading Float
  | Loaded Load
  | EditingBook Load Book


type Msg = Nil 
  | LoadNotes -- Request notes from API
  | Select Book -- Select notes from a book
   -- Select file to upload notes
  | SelectFile
  | FileSelected File
  | FileLoaded String
  | Uploading Http.Progress
  | GotNotes (Result Http.Error (List Note)) -- Notes uploaded
  -- Remove note by id
  | RemoveNote Int 
  | Removed (Result Http.Error ())
  -- Edit book info
  | EditBook 
  | UpdateTitle Title
  | UpdateAuthor Author
  | SaveBook 
  | BookUpdated (Result Http.Error ())
  | Cancel 


apiUrl : String
apiUrl = "http://localhost:3000"


init : () -> (Model, Cmd Msg)
init _ =
  ( New, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    -- Http.track "upload" UploadProgress
  Sub.none


-- HTTP

getNotesAPI : Cmd Msg
getNotesAPI =
  Http.get
    { url = Builder.crossOrigin apiUrl ["notes"] []
    , expect = Http.expectJson GotNotes (list noteDecoder)
    }


deleteNoteAPI : Int -> Cmd Msg
deleteNoteAPI id =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = Builder.crossOrigin apiUrl ["notes"] [Builder.int "id" id]
    , body = Http.emptyBody -- Encode.object [("id", Encode.int id)] |> Http.jsonBody
    , expect = Http.expectWhatever Removed
    , timeout = Nothing
    , tracker = Just "upload"
    }


uploadNotesAPI : String -> Cmd Msg
uploadNotesAPI content =
  Http.request
    { method = "POST"
    , headers = []
    , url = Builder.crossOrigin apiUrl ["notes"] []
    , body = Http.stringBody "text/plain" content 
    , expect = Http.expectJson GotNotes (list noteDecoder)
    , timeout = Nothing
    , tracker = Just "upload"
    }


updateBookAPI : Book -> Book -> Cmd Msg
updateBookAPI oldBk newBk =
  Http.request
    { method = "POST"
    , headers = []
    , url = Builder.crossOrigin apiUrl ["update", "book"] []
    , body = Http.jsonBody (E.list bookEncoder [oldBk, newBk])
    , expect = Http.expectWhatever BookUpdated
    , timeout = Nothing
    , tracker = Just "upload"
    }

-- UPDATE

makeNotes : List Note -> Model
makeNotes notes =
  let
      bks = List.map getBook (uniqueBy getTitle notes)
      (book, initNts) = initNotes bks notes
  in
  case book of
      Nothing -> New
      Just bk -> Loaded {notes = notes, selectedNotes = initNts, books = bks, selectedBook = bk}


filterRemovedNote : Load -> Int -> Load
filterRemovedNote load id = 
    {load | 
    notes = filter (\x -> ((getId x) /= id)) (load.notes), 
    selectedNotes = filter (\x -> ((getId x) /= id)) (load.selectedNotes)
    }


editBook : Book -> Book -> Book -> Book
editBook b1 b2 updatedBk = 
  case b1==b2 of
    False -> b1
    True -> updatedBk 


updateBookInfo : Load -> Book -> Model
updateBookInfo load updatedBk  = 
  let
    sbk = load.selectedBook
    newNotes = List.map (\n -> {n | book = (editBook n.book sbk updatedBk )}) load.notes
  in
    Loaded { notes = newNotes
    , selectedNotes = filterNotes updatedBk  newNotes 
    , books = List.map (\b -> editBook b sbk updatedBk ) load.books
    , selectedBook = updatedBk }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
      tmp = (msg, model)
  in
  case tmp of
    -- User select book to see list of notes
    (Select bk, Loaded load) -> (Loaded {load | selectedNotes = filterNotes bk load.notes, selectedBook = bk}, Cmd.none)

    -- Removes one note
    (RemoveNote id, Loaded load )-> (Loaded (filterRemovedNote load id), deleteNoteAPI id)

    -- Upload of the clippings.txt
    (SelectFile, _ ) -> (model, Select.file ["text"] FileSelected)
    (FileSelected file, _ ) -> (model, Task.perform FileLoaded (File.toString file))
    (Uploading progress , _ ) -> 
      case progress of 
        Http.Sending p -> (Loading (Http.fractionSent p) , Cmd.none)
        Http.Receiving _ -> (model, Cmd.none)
    (FileLoaded content, _) -> (model, uploadNotesAPI content)
    
    -- Get list of notes from server
    (LoadNotes, _ ) -> (model, getNotesAPI)
    (GotNotes result, _ ) ->
      case result of
        Ok notes ->  (makeNotes notes, Cmd.none)
        Err err -> (Failure err, Cmd.none)

    -- Book info editing and update
    (EditBook, Loaded load ) -> (EditingBook load load.selectedBook, Cmd.none)
    (UpdateTitle t, EditingBook load book ) -> (EditingBook load (newBook t (getBookAuthor book)) , Cmd.none)
    (UpdateAuthor a, EditingBook load book) -> (EditingBook load (newBook (getBookTitle book) a), Cmd.none)
    (SaveBook, EditingBook load book) -> (model, updateBookAPI load.selectedBook book)
    (BookUpdated result, EditingBook load book) ->
      case result of
        Ok _ ->  (updateBookInfo load book, Cmd.none)
        Err err -> (Loaded load, Cmd.none)
    (Cancel, EditingBook load _ ) -> (Loaded load, Cmd.none)

    -- Otherwise do nothing 
    ( _ , _ ) -> (model, Cmd.none)


-- VIEW

view : Model -> Styled.Html Msg
view model =
  case model of
    New -> emptyView |> mainView
    Loading progress -> progress |> uploadingView |> mainView
    Failure code -> code |> errorView |> mainView 
    Loaded load -> load |> loadedView |> mainView 
    EditingBook load book -> (load, book) |> loadedViewEditBook |> mainView 


makeNoteView : Note -> Styled.Html Msg
makeNoteView note = 
  Styled.div [S.noteDiv] 
    [ Styled.span [Attr.css [color (hex "#606060")]] [Styled.text (getInfo note) ]
    , Styled.button ([S.button] ++ [Event.onClick (RemoveNote note.id)]) [Styled.text "Remove"]
    , Styled.p [] [Styled.text note.body]
    ]


makeNoteHeader : Book -> Styled.Html Msg
makeNoteHeader bk = 
  Styled.div [S.boldHeader]
    [ Styled.text (getBookTitle bk)
    , Styled.div [Attr.css [color (hex "#606060")]] [Styled.text (getBookAuthor bk)]
    , Styled.button ([S.button] ++ [Event.onClick EditBook]) [Styled.text "Edit"]
    ]


editNoteHeader : Book -> Styled.Html Msg
editNoteHeader book = 
  let
      title = getBookTitle book
      author = getBookAuthor book
  in
  Styled.div [S.boldHeader]
    [ Styled.textarea 
        ([Attr.value title, Event.onInput UpdateTitle] ++ [S.bigTextArea]) 
        [Styled.text title] 
    , Styled.textarea 
        ([Attr.value author, Event.onInput UpdateAuthor] ++ [S.bigTextArea] ++ [Attr.css [color (hex "#606060")]]) 
        [Styled.text author]
    , Styled.button ([S.button] ++ [Event.onClick SaveBook]) [Styled.text "Save"]
    , Styled.button ([S.button] ++ [Event.onClick Cancel]) [Styled.text "Cancel"]
    ]


makeBookView : Book -> Book -> Styled.Html Msg
makeBookView book selectedBook =  
  let
      stl = if book==selectedBook then [Attr.css [fontWeight bold]] else []
  in
  Styled.li 
    [ Attr.css [listStyle none] ]
    [ Styled.button 
        ( stl ++ [S.btnText] ++ [Event.onClick (Select book) ]) 
        [Styled.text (getBookTitle book) ]
    ]


mainView : List (Styled.Html Msg) -> Styled.Html Msg
mainView v = 
    Styled.div 
        [S.mainDivLight] 
        v


loadedView : Load -> List (Styled.Html Msg)
loadedView load = 
  [ Styled.div [S.leftMenu] 
    [ Styled.div [S.navBarLogo] [Styled.text "Knotes"]
    , Styled.ul [Attr.css [paddingTop (px 60)]] (List.map (\b -> makeBookView b load.selectedBook) (sortBy .title load.books)) ]
  , Styled.div [S.notesView] 
    [ makeNoteHeader load.selectedBook
    , Styled.div [S.notesList] (List.map makeNoteView load.selectedNotes)]
  ]


loadedViewEditBook : (Load, Book) -> List (Styled.Html Msg)
loadedViewEditBook (load, book) = 
  [ Styled.div [S.leftMenu] 
    [ Styled.div [S.navBarLogo] [Styled.text "Knotes"]
    , Styled.ul [Attr.css [paddingTop (px 60)]] (List.map (\b -> makeBookView b load.selectedBook) load.books)  ]
  , Styled.div [S.notesView] 
    [ editNoteHeader book
    , Styled.div [S.notesList] (List.map makeNoteView load.selectedNotes)]
  ]


emptyView : List (Styled.Html Msg)
emptyView =   
  [ Styled.div [S.emptyView] 
    [ Styled.div [S.emptyLogo] [Styled.text "Knotes"]
    , Styled.button ([S.bigButton] ++ [Event.onClick LoadNotes]) [Styled.text "Load notes!"]
    , Styled.button ([S.bigButton] ++ [Event.onClick SelectFile]) [Styled.text "Upload notes"]
    ]
  ]


uploadingView : Float -> List (Styled.Html Msg)
uploadingView progress =   
  [ Styled.div [S.emptyView] 
    [ Styled.div [S.emptyLogo] [Styled.text "Knotes"]
    , Styled.button ([S.bigButton] ++ [Event.onClick LoadNotes]) [Styled.text "Load notes!"]
    , Styled.button ([S.bigButton] ++ [Event.onClick SelectFile]) [Styled.text "Upload notes"]
    , Styled.div [] [Styled.text ("Uploading prgoress: " ++ (Debug.toString progress))]
    ]
  ]


errorView : Error -> List (Styled.Html Msg)
errorView code =   
  [ Styled.div [S.emptyView] 
    [ Styled.div [S.emptyLogo] [Styled.text "Knotes"]
    , Styled.button ([S.bigButton] ++ [Event.onClick LoadNotes]) [Styled.text "Load notes!"]
    , Styled.button ([S.bigButton] ++ [Event.onClick SelectFile]) [Styled.text "Upload notes"]
    , Styled.div [Attr.css [color (hex "#606060"), fontSize (pt 12)]] 
      [Styled.text ("Whoops! Something went wrong! Error: " ++ (code |> toString |> String.toLower))]
    ]
  ]
