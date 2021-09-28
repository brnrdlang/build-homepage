{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module CSS
    ( assembleHomeCSS,
      assembleScienceCSS,
      assembleArtCSS
    ) where

import Prelude hiding (div)
import Clay
import qualified Clay.Flexbox as FB

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

gridBody :: Css
gridBody =
  body ?
    do background black
       display    grid
       "grid-template-columns" -: "[start] 0.75fr [left] 1fr [center] 1fr [right] 0.75fr [end]"
       sym margin (px 0.0)
       minHeight (vh 100.0)
       textAlign center

flexBody :: Css
flexBody = 
  body ?
    do background black
       display    flex
       FB.flexWrap FB.wrap
       sym margin (px 0.0)
       minHeight (vh 100.0)
       textAlign center

commonCSSCode :: Css
commonCSSCode = do
  importUrl "'https://fonts.googleapis.com/css2?family=Fira+Sans:wght@300&family=Vollkorn&display=swap'"

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
  flexBody
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
       fontFamily ["Vollkorn"] [serif]
       padding (em 0) (em 4) (em 2) (em 4)
       FB.flex 1 1 (em 25)

  styleLinks ".science" $ scienceTextColor
  
  ".art" ?
    do background artBgColor
       color      artTextColor
       fontFamily ["Fira Sans"] [sansSerif]       
       padding (em 0) (em 4) (em 0) (em 4)
       FB.flex 1 1 (em 25)
       
  styleLinks ".art" $ artTextColor
  
  ".art" ? div ?
    do marginTop (em 2)
       padding (em 2) (em 2) (em 2) (em 2)
       marginLeft auto
       marginRight auto
       border solid (px 1) white
       maxWidth (em 22)

menuCSS :: Css
menuCSS = do
  ".scienceMenu" ?
    do background scienceBgColor
       color      scienceTextColor
       FB.flex    1 1 auto
       padding (em 2) (em 4) (em 0) (em 4)

  styleLinks ".scienceMenu" $ scienceTextColor

  ".scienceMenu" ? div ?
    do width (pct 100.0)
       fontSize (em 1.5)
       margin (em 0.5) (em 0) (em 0.5) (em 0)
  
  ".scienceLink" ?
    do fontFamily ["Vollkorn"] [serif]

  ".scienceLink" ? a ?
   do textDecoration none
     
  ".artMenu" ?
    do background artBgColor
       color      artTextColor
       FB.flex    1 1 auto
       padding (em 2) (em 4) (em 0) (em 4)

  styleLinks ".artMenu" $ artTextColor
 
  ".artMenu" ? div ?
    do width (pct 100.0)
       fontSize (em 1.5)
       margin (em 0.5) (em 0) (em 0.5) (em 0)
       
  ".artLink" ?
    do fontFamily ["Fira Sans"] [sansSerif]
 
  ".artLink" ? a ?
    do textDecoration none

assembleScienceCSS :: Css
assembleScienceCSS = do
  flexBody
  commonCSSCode
  menuCSS
  
  ".scienceContent" ?
    do background scienceBgColor
       color      scienceTextColor
       FB.flex       3 1 (em 60)
       fontFamily ["Vollkorn"] [serif]
       padding (em 0) (em 4) (em 0) (em 4)

  styleLinks ".scienceContent" $ scienceTextColor

assembleArtCSS :: Css
assembleArtCSS = do
  flexBody
  commonCSSCode
  menuCSS
    
  ".artContent" ?
    do background artBgColor
       color      artTextColor
       FB.flex       3 1 (em 60)
       fontFamily ["Fira Sans"] [sansSerif]       
       padding (em 0) (em 4) (em 0) (em 4)

  styleLinks ".artContent" $ artTextColor
  
  ".artContent" ? img ?
    do display block
       marginLeft auto
       marginRight auto
       maxWidth (pct 85)
       marginTop (em 2)
       marginBottom (em 2)
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
