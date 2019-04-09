module Style            exposing (..)
import Html             exposing (Attribute)
import Html.Attributes  exposing (style)
import Html.Events      as Event


makeStyle : List (String, String) -> List (Attribute a)
makeStyle xs = List.map (\(x, y) -> style x y) xs

-- Main view

mainDivLight : List (Attribute a)
mainDivLight = makeStyle 
    [ ("min-height", "100%")
    , ("min-width", "100%")
    , ("font-family", "arial")
    , ("position", "relative")]

-- Navbar view

navBarLogo : List (Attribute a)
navBarLogo = makeStyle
    [ ("font-family", "arial")
    , ("font-weight", "bold")
    , ("font-size", "42pt")
    , ("text-align", "center")
    , ("position", "fixed")
    , ("background-color", "white")
    , ("width", "15vw")
    , ("height", "7vh")]


leftMenu : List (Attribute a)
leftMenu = makeStyle
    [ ("width", "15vw")
    , ("height", "100%")
    , ("position", "fixed")    
    , ("border", "4px")
    , ("border-style", "none solid none none")
    , ("border-color", "#C0C0C0")
    , ("overflow-y", "scroll")
    , ("padding-right", "30px")
    , ("box-sizing", "content-box")]


btnText : List (Attribute a) 
btnText = makeStyle  
    [ ("border", "none")
    , ("background-color", "inherit")
    , ("padding", "10px")
    , ("font-size", "12pt")
    , ("cursor", "pointer")
    , ("width", "100%")
    , ("text-align", "left")
    , ("color", "black")
    ]

-- Notes view

notesView : List (Attribute a)
notesView = makeStyle
    [ ("float", "right")
    , ("margin-right", "5vw")
    , ("width", "70vw")]


boldHeader : List (Attribute a)
boldHeader = makeStyle
    [ ("font-family", "arial")
    , ("font-weight", "bold")
    , ("font-size", "42pt")
    , ("text-align", "left")
    , ("position", "relative")
    , ("height", "auto")
    , ("overflow-y", "auto")
    , ("padding-bottom", "50px")
    , ("overflow-x", "hidden")]


bigTextArea: List (Attribute a)
bigTextArea = makeStyle
    [ ("font-family", "arial")
    , ("font-weight", "bold")
    , ("font-size", "42pt")
    , ("text-align", "left")
    , ("width", "100%")
    , ("min-height", "46pt")
    , ("resize", "none")
    , ("border", "none")
    , ("overflow", "hidden")]


notesList : List (Attribute a)
notesList = makeStyle
    [ ("overflow-y", "auto")
    , ("position", "relative")]


noteDiv : List (Attribute a)
noteDiv = makeStyle  
    [ ("padding-bottom", "20px")
    , ("text-align", "left") ]
    

button : List (Attribute a)
button = makeStyle
    [ ("text-transform", "uppercase")
    , ("background", "#ffffff")
    , ("font-size", "12pt")
    , ("color", "#C0C0C0")
    , ("cursor", "pointer")
    , ("border", "none")
    ]

-- Empty view 

emptyView : List (Attribute a)
emptyView = makeStyle
    [ ("text-align", "center")
    , ("font-size", "24pt")
    , ("width", "100%")
    , ("height", "300px")
    ]


emptyLogo : List (Attribute a)
emptyLogo = makeStyle
    [ ("font-family", "arial")
    , ("font-weight", "bold")
    , ("font-size", "144pt")
    , ("text-align", "center")]


bigButton: List (Attribute a)
bigButton = makeStyle
    [ ("text-transform", "uppercase")
    , ("background", "inherit")
    , ("font-size", "24pt")
    , ("color", "#C0C0C0")
    , ("cursor", "pointer")
    , ("border", "none")]
