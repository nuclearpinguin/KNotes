module Style            exposing (..)
import Html             exposing (Attribute)
import Html.Attributes  exposing (style)


makeStyle : List (String, String) -> List (Attribute a)
makeStyle xs = List.map (\(x, y) -> style x y) xs


mainDivLight : List (Attribute a)
mainDivLight = makeStyle 
    [ ("height", "100%")
    , ("width", "100%")
    , ("font-family", "arial")
    , ("position", "absolute")]


-- mainDivDark : List (Attribute a)
-- mainDivDark = makeStyle 
--     [ ("height", "100%")
--     , ("width", "100%")
--     , ("font-family", "arial")
--     , ("overflow", "hidden")
--     , ("position", "relative")
--     , ("background-color", "#1e1e1e")
--     , ("color", "#9f9f9f")]


navBar : List (Attribute a)
navBar = makeStyle
    [ ("position", "fixed")
    , ("width", "100%")
    , ("height", "4vh")
    , ("overflow", "visible")] 


navBarLogo : List (Attribute a)
navBarLogo = makeStyle
    [ ("font-family", "arial")
    , ("font-size", "24pt")
    , ("margin-left", "40px")
    , ("margin-top", "4px")]


leftMenu : List (Attribute a)
leftMenu = makeStyle
    [ ("width", "15vw")
    , ("height", "100%")
    , ("position", "fixed")    
    , ("border", "4px")
    , ("border-style", "none solid none none")
    , ("border-color", "#C0C0C0")
    -- , ("overflow-y", "hidden")
    , ("overflow", "auto") ]


btnText : List (Attribute a) 
btnText = makeStyle  
    [ ("border", "none")
    , ("background-color", "inherit")
    , ("padding", "10px")
    , ("font-size", "12pt")
    , ("cursor", "pointer")
    , ("width", "100%")
    -- , ("display", "inline-block")
    , ("text-align", "left")
    ]


notesList : List (Attribute a)
notesList = makeStyle
    [ ("float", "right")
    , ("margin-right", "5vw")
    , ("width", "70vw")
    , ("height", "100%")
    , ("overflow", "auto")]


noteDiv : List (Attribute a)
noteDiv = makeStyle  
    [ ("padding-bottom", "20px")
    , ("text-align", "left") ]
    