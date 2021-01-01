{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lib
    ( assembleDocument,
      homeBody,
      scienceBody,
      artBody
    ) where

import Lucid

import Data.Text(Text)

assembleDocument :: Html () -> Text -> Html () -> Html ()
assembleDocument title csspath body = doctypehtml_ $ do
  head_ $ do
    title_ title
    meta_ [charset_ "utf-8"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ csspath]
  body_ body

scienceText :: Html ()
scienceText = 
  "I am a scientist working on visual perception. \
  \My current research interests are the varying aspects \
  \of the perception of surfaces: material, shape, illumination. \
  \Furthermore I am working on statistical models \
  \for the analysis of psychophysical data."

blogText :: Html ()
blogText =
  "Here I provide you with articles about topics I am interested in."

homeBody :: Html()
homeBody = do
  div_ [class_ "left"] $ do
    h1_ "Bernhard Lang"
    div_ $ img_ [src_ "resources/bl_500.png"]
    p_ [style_ "text-align:left"] scienceText
    h2_ . a_ [href_ "science.html"] $ "My Blog"
    p_ [style_ "text-align:left"] blogText
    h2_ "Curriculum Vitae"
    table_ (tr_ (td_ [class_ "dateRow"] "2018 - Now"
              <> td_ "Phd student, University of Tübingen")
         <> tr_ (td_ [class_ "dateRow"] "2015 - 2017"
              <> td_ "M.Sc. in Neural Information Processing, University of Tübingen")
         <> tr_ (td_ [class_ "dateRow"] "2012 - 2015"
              <> td_ "B.Sc. in Computational Molecular Biology, Saarland University"))
  div_ [class_ "right"] $ do
    h1_ "Achromatic"
    p_ "Photography, drawing, computer graphics, music, animation, games."
    div_ $ (img_ [src_ "resources/salamander_wm.jpg", style_ "width: 12em;margin-right: 1em;margin-bottom:3em;"]
            <> img_ [src_ "resources/wm_8-August.jpg", style_ "width: 9em;"]
            <> br_ []
            <> img_ [src_ "resources/Bläuling_schmetterling_wm.jpg", style_ "width: 9em;margin-right: 1em;margin-top:-2em;"]
            <> img_ [src_ "resources/3-März.JPG", style_ "width: 12em;"])

scienceBody :: Html() -> Html () -> Html ()
scienceBody menu content = do
  div_ [class_ "leftContent"] content
  div_ [class_ "rightMenu"] menu

artBody :: Html() -> Html () -> Html ()
artBody menu content = do
  div_ [class_ "leftMenu"] menu
  div_ [class_ "rightContent"] content
