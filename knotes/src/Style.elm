module Style            exposing (..)
import Html

import Css              exposing (..)
import Html.Styled      exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events     exposing (onClick)


-- Main view
mainDivLight : Attribute a
mainDivLight = css
    [ minHeight (pct 100)
    , minWidth (pct 100)
    , fontFamilies [ "Arial" ]
    , position relative ]


-- Navbar view

navBarLogo : Attribute a
navBarLogo = css
    [ fontWeight bold
    , fontSize (pt 42)
    , textAlign center
    , position fixed
    , backgroundColor (hex "FFFFFF")
    , width (vw 15)
    , height (vh 7)]



leftMenu : Attribute a
leftMenu = css
    [ width (vw 15)
    , height (pct 100)
    , position fixed
    , borderRight2 (px 4) solid 
    , borderColor (hex "#C0C0C0")
    , overflowY scroll
    , boxSizing contentBox]


btnText : Attribute a 
btnText = css
    [ border (px 0)
    , backgroundColor inherit
    , padding (px 10)
    , fontSize (pt 12)
    , cursor pointer
    , width (pct 100)
    , textAlign left
    , color (hex "#0000000")
    , hover [fontWeight bold]
    ]

-- Notes view

notesView : Attribute a
notesView = css
    [ float right
    , marginRight (vw 5)
    , width (vw 70)]

boldHeader : Attribute a
boldHeader = css
    [ fontFamilies [ "Arial" ]
    , fontWeight bold
    , fontSize (pt 42)
    , textAlign left
    , position relative
    , height auto
    , overflowY auto
    , paddingBottom (px 50)
    , overflowX hidden]


bigTextArea: Attribute a
bigTextArea = css
    [ fontFamilies [ "Arial" ]
    , fontWeight bold
    , fontSize (pt 42)
    , textAlign left
    , width (pct 100)
    , minHeight (pt 46)
    , resize none
    , border (px 0)
    , overflow hidden]


notesList : Attribute a
notesList = css
    [ overflowY auto
    , position relative]


noteDiv : Attribute a
noteDiv = css  
    [ paddingBottom (px 20)
    , textAlign left ]
    

button : Attribute a
button = css
    [ textTransform uppercase
    , backgroundColor (hex "#ffffff")
    , fontSize (pt 12)
    , color  (hex "#C0C0C0")
    , cursor pointer
    , border (px 0)
    , hover [fontWeight bold]
    ]

-- Empty view 

emptyView : Attribute a
emptyView = css
    [ textAlign center
    , fontSize (pt 24)
    , width (pct 100)
    , height (px 300)
    ]


emptyLogo : Attribute a
emptyLogo = css
    [ fontFamilies [ "Arial" ]
    , fontWeight bold
    , fontSize (pt 144)
    , textAlign center]


bigButton : Attribute a
bigButton = css
    [ textTransform uppercase
    , backgroundColor inherit
    , fontSize (pt 24)
    , color (hex "#C0C0C0")
    , cursor pointer
    , border (px 0)
    , hover [fontWeight bold]]

