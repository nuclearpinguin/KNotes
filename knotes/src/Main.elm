module Main             exposing ( .. )

import Debug            exposing ( log, toString )
import List             exposing  ( .. )
import Browser
import Html             exposing  ( Html, Attribute )
import Html.Attributes  as Attr
import Html.Events      as Event
import List.Extra       exposing ( uniqueBy )

-- Files,http and encode / decode
import Json.Encode      as Encode
import Json.Decode      as D exposing (Decoder, decodeString, field, string, list, int, map2, map4 )
import Http             exposing ( Error )
import Url.Builder      as Builder
import File.Select      as Select
import File             exposing (File)
import Task

-- Custom imports
import Style

main =
  Browser.element 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL
type alias Book = 
  { author : String
  , title : String}


type alias Note = 
  { book: Book
  , body: String
  , info: String
  , id: Int
  }

type alias Load = 
  { notes: List Note
  , selectedNotes: List Note
  , books: List Book
  }

type Model = New 
  | Loading 
  | Failure Error 
  | Loaded Load


type Msg = Nil 
  | SelectFile 
  | FileSelected File
  | FileLoaded String
  -- | UploadProgress Http.Progress
  | Select Book
  | LoadNotes
  | GotNotes (Result Http.Error (List Note)) 
  | RemoveNote Int
  | Removed (Result Http.Error ())


apiUrl : String
apiUrl = "http://localhost:3000"


init : () -> (Model, Cmd Msg)
init _ =
  ( New
  , Cmd.none
  )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Http.track "upload" UploadProgress
  Sub.none


-- HTTP

bookDecoder : Decoder Book
bookDecoder = 
  map2 Book  
    (field "author" string)
    (field "title" string)


noteDecoder : Decoder Note
noteDecoder = 
  map4 Note
    (field "book" bookDecoder)
    (field "body" string)
    (field "info" string)
    (field "id" int)


getNotes : Cmd Msg
getNotes =
  Http.get
    { url = Builder.crossOrigin apiUrl ["notes"] []
    , expect = Http.expectJson GotNotes (list noteDecoder)
    }


deleteNote : Int -> Cmd Msg
deleteNote id =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = Builder.crossOrigin apiUrl ["notes"] [Builder.int "id" id]
    , body = Http.emptyBody -- Encode.object [("id", Encode.int id)] |> Http.jsonBody
    , expect = Http.expectWhatever Removed
    , timeout = Nothing
    , tracker = Nothing
    }

uploadNotes : String -> Cmd Msg
uploadNotes content =
  Http.request
    { method = "POST"
    , headers = []
    , url = Builder.crossOrigin apiUrl ["notes"] []
    , body = Http.stringBody "text/plain" content 
    , expect = Http.expectJson GotNotes (list noteDecoder)
    , timeout = Nothing
    , tracker = Just "upload"
    }

-- UPDATE

filterBooks : Book -> List Note -> List Note
filterBooks bk nts = filter (\n -> n.book == bk) nts


makeNotes : List Note -> Model
makeNotes notes =
  let
      bks = notes |> map (\n -> n.book) |> uniqueBy (\b -> b.title)
      initNts = initNotes bks notes
  in
    Loaded {notes = notes, selectedNotes = initNts, books = bks}


initNotes : List Book -> List Note -> List Note
initNotes bks nts = 
  case List.head bks of
    Nothing -> []
    Just bk -> filterBooks bk nts


filterRemovedNote : Load -> Int -> Load
filterRemovedNote load id = 
    {load | 
    notes = filter (\x -> (x.id /= id)) (load.notes), 
    selectedNotes = filter (\x -> (x.id /= id)) (load.selectedNotes)
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
      tmp = (msg, model)
  in
  case tmp of
    (RemoveNote id, Loaded load )-> (Loaded (filterRemovedNote load id), deleteNote id)
    (RemoveNote id, _ )-> (model, deleteNote id)

    (SelectFile, _) -> (model, Select.file ["text"] FileSelected)
    (FileSelected file, _ ) -> (model, Task.perform FileLoaded (File.toString file))
    (FileLoaded content, _) -> (model, uploadNotes content)

    -- (UploadProgress progress , _) -> 
    --   case progress of 
    --     Http.Sending p -> (model , Cmd.none) --Uploading (Http.fractionSent p)
    --     Http.Receiving _ -> (model, Cmd.none)
        
    (Select bk, Loaded load) -> (Loaded {load | selectedNotes = filterBooks bk load.notes}, Cmd.none)
    
    (LoadNotes, _ ) -> (model, getNotes)
    (GotNotes result, _) ->
      case result of
        Ok notes ->  (makeNotes notes, Cmd.none)
        Err err -> (Failure err, Cmd.none)

    (_ , _) -> (model, Cmd.none)

-- VIEW

makeNoteView : Note -> Html Msg
makeNoteView note = 
  Html.div Style.noteDiv 
    [ Html.h4 [] [Html.text note.book.title]
    , Html.span [Attr.style "color" "#C0C0C0"] [Html.text (String.join " | " [note.book.author, note.info]) ]
    , Html.p [] [Html.text note.body]
    , Html.button [Event.onClick (RemoveNote note.id)] [Html.text "Remove"]
    ]


makeBookView : Book -> Html Msg
makeBookView book = 
  Html.li 
    [Attr.style "list-style" "none"] 
    [ Html.button ( Style.btnText ++ [Event.onClick (Select book) ]) [Html.text book.title ]]


mainView : List (Html Msg) -> Html Msg
mainView v = 
    Html.div Style.mainDivLight
    [ Html.nav Style.navBar [Html.h1 Style.navBarLogo [Html.text "KNotes"]]
    , Html.div [Attr.style "margin-top" "50px"] v
    ]


loadedView : Load -> List (Html Msg)
loadedView load = 
  [ Html.div Style.leftMenu [ Html.ul [] (map makeBookView load.books) ]
  , Html.div Style.notesList (map makeNoteView load.selectedNotes)
  ]


emptyView : List (Html Msg)
emptyView =   
  [ Html.div Style.leftMenu [ Html.ul [] [Html.text "No books yet."] ] 
  , Html.div Style.notesList 
    [ Html.text "Load notes"
    , Html.button [Event.onClick LoadNotes] [Html.text "Load!"]
    , Html.text "Upload notes"
    , Html.button [Event.onClick SelectFile] [Html.text "Select"]
    ]
  ]


errorView : Error -> List (Html Msg)
errorView code =   
  [ Html.div Style.leftMenu [ Html.ul [] [Html.text "No books yet."] ] 
  , Html.div Style.notesList 
    [ Html.text ("Whoops! Something went wrong! Error: " ++ (code |> toString |> String.toLower))]
  ]


view : Model -> Html Msg
view model =
  case model of
    New -> emptyView |> mainView
    Loading -> emptyView |> mainView
    Failure code -> code |> errorView |> mainView 
    Loaded load-> load |> loadedView |> mainView 

