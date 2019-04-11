module Main             exposing ( .. )

import Debug            exposing ( log, toString )
import List             exposing  ( .. )
import Browser
import Html             exposing  ( Html, Attribute )
import Html.Attributes  as Attr
import Html.Events      as Event
import List.Extra       exposing ( uniqueBy )

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
    , view = view
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
      bks = map getBook (uniqueBy getTitle notes)
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
    newNotes = map (\n -> {n | book = (editBook n.book sbk updatedBk )}) load.notes
  in
    Loaded { notes = newNotes
    , selectedNotes = filterNotes updatedBk  newNotes 
    , books = map (\b -> editBook b sbk updatedBk ) load.books
    , selectedBook = updatedBk }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
      tmp = (msg, model)
  in
  case tmp of
    (Select bk, Loaded load) -> (Loaded {load | selectedNotes = filterNotes bk load.notes, selectedBook = bk}, Cmd.none)

    (RemoveNote id, Loaded load )-> (Loaded (filterRemovedNote load id), deleteNoteAPI id)
    (RemoveNote id, _ )-> (model, deleteNoteAPI id)

    (SelectFile, _) -> (model, Select.file ["text"] FileSelected)
    (FileSelected file, _ ) -> (model, Task.perform FileLoaded (File.toString file))
    (Uploading progress , _) -> 
      case progress of 
        Http.Sending p -> (Loading (Http.fractionSent p) , Cmd.none)
        Http.Receiving _ -> (model, Cmd.none)
    (FileLoaded content, _) -> (model, uploadNotesAPI content)
    
    (LoadNotes, _ ) -> (model, getNotesAPI)
    (GotNotes result, _) ->
      case result of
        Ok notes ->  (makeNotes notes, Cmd.none)
        Err err -> (Failure err, Cmd.none)

    (EditBook, Loaded load ) -> (EditingBook load load.selectedBook, Cmd.none)
    (UpdateTitle t, EditingBook load book ) -> (EditingBook load (newBook t (getBookAuthor book)) , Cmd.none)
    (UpdateAuthor a, EditingBook load book) -> (EditingBook load (newBook (getBookTitle book) a), Cmd.none)
    (SaveBook, EditingBook load book) -> (model, updateBookAPI load.selectedBook book)
    (BookUpdated result, EditingBook load book) ->
      case result of
        Ok _ ->  (updateBookInfo load book, Cmd.none)
        Err err -> (Loaded load, Cmd.none)
    (Cancel, EditingBook load _) -> (Loaded load, Cmd.none)

    (_ , _) -> (model, Cmd.none)


-- VIEW
view : Model -> Html Msg
view model =
  case model of
    New -> emptyView |> mainView
    Loading progress -> progress |> uploadingView |> mainView
    Failure code -> code |> errorView |> mainView 
    Loaded load -> load |> loadedView |> mainView 
    EditingBook load book -> (load, book) |> loadedViewEditBook |> mainView 


makeNoteView : Note -> Html Msg
makeNoteView note = 
  Html.div S.noteDiv 
    [ Html.span [Attr.style "color" "#606060"] [Html.text (getInfo note) ]
    , Html.button (S.button ++ [Event.onClick (RemoveNote note.id)]) [Html.text "Remove"]
    , Html.p [] [Html.text note.body]
    ]


makeNoteHeader : Book -> Html Msg
makeNoteHeader bk = 
  Html.div S.boldHeader
    [ Html.text (getBookTitle bk)
    , Html.div [Attr.style "color" "#606060"] [Html.text (getBookAuthor bk)]
    , Html.button (S.button ++ [Event.onClick EditBook]) [Html.text "Edit"]
    ]


editNoteHeader : Book -> Html Msg
editNoteHeader book = 
  let
      title = getBookTitle book
      author = getBookAuthor book
  in
  Html.div S.boldHeader
    [ Html.textarea 
        ([Attr.value title, Event.onInput UpdateTitle] ++ S.bigTextArea) 
        [Html.text title] 
    , Html.textarea 
        ([Attr.value author, Event.onInput UpdateAuthor] ++ S.bigTextArea ++ [Attr.style "color" "#606060"]) 
        [Html.text author]
    , Html.button (S.button ++ [Event.onClick SaveBook]) [Html.text "Save"]
    , Html.button (S.button ++ [Event.onClick Cancel]) [Html.text "Cancel"]
    ]


makeBookView: Book -> Book-> Html Msg
makeBookView book selectedBook =  
  let
      stl = if book==selectedBook then [Attr.style "font-weight" "bold"] else []
  in
  Html.li 
    [Attr.style "list-style" "none"]
    [ Html.button ( stl ++ S.btnText ++ [Event.onClick (Select book) ]) [Html.text (getBookTitle book) ]]


mainView : List (Html Msg) -> Html Msg
mainView v = 
    Html.div S.mainDivLight v


loadedView : Load -> List (Html Msg)
loadedView load = 
  [ Html.div S.leftMenu 
    [ Html.div S.navBarLogo [Html.text "Knotes"]
    , Html.ul [Attr.style "padding-top" "60px"] (map (\b -> makeBookView b load.selectedBook) (sortBy .title load.books)) ]
  , Html.div S.notesView 
    [ makeNoteHeader load.selectedBook
    , Html.div S.notesList (map makeNoteView load.selectedNotes)]
  ]


loadedViewEditBook : (Load, Book) -> List (Html Msg)
loadedViewEditBook (load, book) = 
  [ Html.div S.leftMenu 
    [ Html.div S.navBarLogo [Html.text "Knotes"]
    , Html.ul [Attr.style "padding-top" "60px"] (map (\b -> makeBookView b load.selectedBook) load.books)  ]
  , Html.div S.notesView 
    [ editNoteHeader book
    , Html.div S.notesList (map makeNoteView load.selectedNotes)]
  ]


emptyView : List (Html Msg)
emptyView =   
  [ Html.div S.emptyView 
    [ Html.div S.emptyLogo [Html.text "Knotes"]
    , Html.button (S.bigButton ++ [Event.onClick LoadNotes]) [Html.text "Load notes!"]
    , Html.button (S.bigButton ++ [Event.onClick SelectFile]) [Html.text "Upload notes"]
    ]
  ]


uploadingView : Float -> List (Html Msg)
uploadingView progress =   
  [ Html.div S.emptyView 
    [ Html.div S.emptyLogo [Html.text "Knotes"]
    , Html.button (S.bigButton ++ [Event.onClick LoadNotes]) [Html.text "Load notes!"]
    , Html.button (S.bigButton ++ [Event.onClick SelectFile]) [Html.text "Upload notes"]
    , Html.div [] [Html.text ("Uploading prgoress: " ++ (Debug.toString progress))]
    ]
  ]


errorView : Error -> List (Html Msg)
errorView code =   
  [ Html.div S.emptyView 
    [ Html.div S.emptyLogo [Html.text "Knotes"]
    , Html.button (S.bigButton ++ [Event.onClick LoadNotes]) [Html.text "Load notes!"]
    , Html.button (S.bigButton ++ [Event.onClick SelectFile]) [Html.text "Upload notes"]
    , Html.div [Attr.style "color" "#606060", Attr.style "font-size" "12pt"] 
      [Html.text ("Whoops! Something went wrong! Error: " ++ (code |> toString |> String.toLower))]
    ]
  ]
