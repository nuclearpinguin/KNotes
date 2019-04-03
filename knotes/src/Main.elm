module Main             exposing ( .. )

import Browser
import Html             exposing  ( Html, Attribute, button
                                  , div, text, ul, li, a, h3, nav, h4, p, span, h2)
import Html.Attributes  exposing  ( style, attribute, hidden, href)
import Html.Events      exposing  ( onClick )
import List             exposing  ( .. )



import Style            


main =
  Browser.sandbox { init = 0, update = update, view = view }

-- MODEL

type Msg = Smth | Other

type alias Model = Int

-- init : Game
-- init = 
--     { board = emptyBoard
--     , player = O
--     , state = NewGame
--     , gameType = Nil
--     }

-- UPDATE


update : Msg -> Model -> Model
update msg model = model + 1

makeNote : Int -> Html Msg
makeNote n = 
  div [style "padding-bottom" "20px", style "text-align" "left"] 
    [ h4 [] [text "Title of the book"]
    , span [style "color" "#C0C0C0"] [text (String.join " | " ["Name Author Jr.", "Some info about the note"]) ]
    , p [] [text """ Read about The Elm Architecture to learn how to use this. Just do it. 
            The additional context is very worthwhile! (Honestly, it is best to just 
            read that guide from front to back instead of muddling around and reading it piecemeal.)"""]
    ]

makeBook : Int -> Html Msg
makeBook n = li [style "list-style" "none"] [ button ( Style.btnText ++ [onClick Smth ]) [text "Book 1"]]


-- VIEW

view : Model -> Html Msg
view model =
  div [style "height" "100%", style "width" "100%", style "font-family" "arial", style "overflow" "hidden", style "position" "relative"]
    [ nav [style "position" "fixed", style "width" "100%"] [h2 [style "margin-left" "40px"] [text "KNotes"]]
    , div [style "margin-top" "50px"] 
        [ div [ style "width" "15vw", style "height" "100%", style "position" "fixed"
        , style "border" "10px", style "border-style" "none solid none none", style "border-color" "#C0C0C0", style "overflow-y" "hidden", style "overflow" "scroll"]
            [ ul [] ([ h3 [] [text "Books"] ] ++ (map makeBook (range 0 100))) ]
        , div [style "float" "right", style "margin-right" "5vw", style "width" "70vw", style "height" "100%"] 
          ([text (String.fromInt model)] ++ map makeNote (range 0 100))
        ]
    ]
