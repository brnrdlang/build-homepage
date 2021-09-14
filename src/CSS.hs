{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module CSS
    ( assembleHomeCSS,
      assembleScienceCSS,
      assembleArtCSS
    ) where

import Prelude hiding (div)
import Clay

-- Constants
-- Colors
scienceBgColor :: Color
scienceBgColor = "#f9f9f9"

scienceTextColor :: Color
scienceTextColor = "#282828"

artBgColor :: Color
artBgColor = "#1e1e1e"

artTextColor :: Color
artTextColor = "#e6e6e6"

styleLinks :: Selector -> Color -> Css
styleLinks sel col = do
  sel ? a # link ?
    do color col
       
  sel ? a # visited ?
    do color col

  sel ? a # hover ?
    do color col

  sel ? a # active ?
    do color col

-- CSS code

commonCSSCode :: Css
commonCSSCode = do
  importUrl "'https://fonts.googleapis.com/css2?family=Fira+Sans:wght@300&family=Vollkorn&display=swap'"

  body ?
    do background black
       display    grid
       "grid-template-columns" -: "[start] 0.75fr [left] 1fr [center] 1fr [right] 0.75fr [end]"
       sym margin (px 0.0)
       minHeight (vh 100.0)
       textAlign center

  element "@font-face" ?
    do fontFamily ["Achromatic WM"] []
       "src"   -: "url(achromatic-wm.otf)"

  p ?
    do maxWidth  (px 600)
       marginLeft auto
       marginRight auto
       textAlign start
       
  ul ?
    do maxWidth  (px 600)
       marginLeft auto
       marginRight auto
       textAlign start

  blockquote ?
    do maxWidth  (px 600)
       marginLeft auto
       marginRight auto
       textAlign justify
       borderLeft solid (em 0.25) "#cccccc"
       paddingLeft (em 1.25)

assembleHomeCSS :: Css
assembleHomeCSS = do
  commonCSSCode
  h1 ?
    do fontFamily ["Achromatic WM"] [sansSerif]
       fontSize   (em 3)
       fontWeight normal
  
  table ?
    do marginLeft auto
       marginRight auto

  td ? textAlign start

  ".dateRow" ?
    do minWidth (em 7)
       height (em 2)

  ".science" ?
    do background scienceBgColor
       color      scienceTextColor
       "grid-column" -: "start / center"
       fontFamily ["Vollkorn"] [serif]
       padding (em 0) (em 4) (em 0) (em 4)

  styleLinks ".science" $ scienceTextColor
  
  ".art" ?
    do background artBgColor
       color      artTextColor
       fontFamily ["Fira Sans"] [sansSerif]       
       "grid-column" -: "center / end"
       padding (em 0) (em 4) (em 0) (em 4)

  styleLinks ".art" $ artTextColor
  
  ".art" ? div ?
    do marginTop (em 2)
       padding (em 2) (em 2) (em 2) (em 2)
       maxWidth (em 22)
       marginLeft auto
       marginRight auto
       border solid (px 1) white

menuCSS :: Css
menuCSS = do
  ".scienceMenu" ?
    do background scienceBgColor
       color      scienceTextColor
       "grid-column" -: "start / left"
       padding (em 2) (em 4) (em 0) (em 4)

  styleLinks ".scienceMenu" $ scienceTextColor

  ".artMenu" ?
    do background artBgColor
       color      artTextColor
       "grid-column" -: "right / end"
       padding (em 2) (em 4) (em 0) (em 4)

  styleLinks ".artMenu" $ artTextColor
  
assembleScienceCSS :: Css
assembleScienceCSS = do
  commonCSSCode
  menuCSS
  
  ".scienceContent" ?
    do background scienceBgColor
       color      scienceTextColor
       "grid-column" -: "start / right"
       fontFamily ["Vollkorn"] [serif]
       padding (em 0) (em 4) (em 0) (em 4)

  styleLinks ".scienceContent" $ scienceTextColor

assembleArtCSS :: Css
assembleArtCSS = do
  commonCSSCode
  menuCSS
    
  ".artContent" ?
    do background artBgColor
       color      artTextColor
       "grid-column" -: "left / end"
       fontFamily ["Fira Sans"] [sansSerif]       
       padding (em 0) (em 4) (em 0) (em 4)

  styleLinks ".artContent" $ artTextColor
  
  ".artContent" ? img ?
    do display block
       marginLeft auto
       marginRight auto

--  ".gallery" ?
--   do maxWidth (px 600)
    
  ".galleryElement" ?
    do float floatLeft
       width (px 250)
       height (px 200)
--       margin (px 5) (px 5) (px 5) (px 5)
       
  ".galleryElement" ? img ?
    do height (px 165)
  
  ".gallery" ? iframe ?
    do float floatLeft
