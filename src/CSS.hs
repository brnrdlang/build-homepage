{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module CSS
    ( assembleCSS
    ) where

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

-- CSS code

assembleCSS :: Css
assembleCSS = do
  importUrl "'https://fonts.googleapis.com/css2?family=Fira+Sans:wght@300&family=Vollkorn&display=swap'"

  body ?
    do background black
       display    grid
       "grid-template-columns" -: "[start] 200px [left] 1fr [center] 1fr [right] 200px [end]"
       sym margin (px 0.0)
       minHeight (vh 100.0)
       textAlign  center
       
  element "@font-face" ?
    do fontFamily ["Achromatic WM"] []
       "src"   -: "url(achromatic-wm.otf)"
  
  h1 ?
    do fontFamily ["Achromatic WM"] [sansSerif]
       fontSize   (em 3)
       fontWeight normal
  
  p ?
    do maxWidth  (px 600)
       marginLeft auto
       marginRight auto

  table ?
    do marginLeft auto
       marginRight auto

  td ? textAlign start

  ".dateRow" ?
    do minWidth (em 7)
       height (em 2)

  ".left" ?
    do background scienceBgColor
       color      scienceTextColor
       "grid-column" -: "start / center"
       fontFamily ["Vollkorn"] [serif]
       padding (em 0) (em 4) (em 0) (em 4)

  ".leftMenu" ?
    do background scienceBgColor
       color      scienceTextColor
       "grid-column" -: "start / left"

  ".leftContent" ?
    do background scienceBgColor
       color      scienceTextColor
       "grid-column" -: "start / right"

  ".right" ?
    do background artBgColor
       color      artTextColor
       fontFamily ["Fira Sans"] [sansSerif]       
       "grid-column" -: "center / end"
       padding (em 0) (em 4) (em 0) (em 4)
       
  ".rightMenu" ?
    do background artBgColor
       color      artTextColor
       "grid-column" -: "right / end"
  
  ".rightContent" ?
    do background artBgColor
       color      artTextColor
       "grid-column" -: "left / end"

