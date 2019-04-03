module Style            exposing (..)

import Html             exposing (Attribute)
import Html.Attributes  exposing (style)

    
btnText : List (Attribute a) 
btnText = 
    [  style "border" "none"
    , style "background-color" "inherit"
    , style "padding" "10px"
    , style "font-size" "12pt"
    , style "cursor" "pointer"
    , style "display" "inline-block"
    ]
    