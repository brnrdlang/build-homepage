{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lucid
import Clay hiding (map)
import Lib
import About
import CSS

import qualified Text.MMark as MM
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import System.FilePath((</>))
import System.Directory(copyFile)

homepageDir = "brnrdlang.github.io/"

itchioWidget :: String -> String
itchioWidget id = "<iframe src=\"https://itch.io/embed/" ++ id ++ "?border_width=0&amp;bg_color=1e1e1e&amp;fg_color=e6e6e6\" width=\"206\" height=\"165\" frameborder=\"0\"><a href=\"https://fullyachromatic.itch.io/flower-pack\">Get the images at itch.io!</a></iframe><div style=\"clear:both;\"></div>"

itchioWidgets :: [(Html (), Html ())]
itchioWidgets = map (\(h, a) -> (toHtml h, toHtmlRaw a)) ([
  ("Insects and other creatures", itchioWidget "877877"),
  ("Flowers and other plants", itchioWidget "875479")
  ] :: [(String, String)])

galleryPaths :: [FilePath]
galleryPaths = [
  "resources/insect-pack",
  "resources/flower-pack"
  ]

gestaltPackage :: IO ()
gestaltPackage = do
  copyFile "gestalt-web/pkg/gestalt.js" "brnrdlang.github.io/resources/js/gestalt.js"
  copyFile "gestalt-web/pkg/gestalt_bg.wasm" "brnrdlang.github.io/resources/js/gestalt_bg.wasm"
--  copyFile "gestalt-web/pkg/gestalt_bg.js" "brnrdlang.github.io/resources/js/gestalt_bg.js"
--  copyFile "gestalt-web/pkg/gestalt.d.ts" "brnrdlang.github.io/resources/js/gestalt.d.ts"
  copyFile "gestalt-web/pkg/package.json" "brnrdlang.github.io/resources/js/package.json"

scienceMenu :: [(String, String)]
scienceMenu = [("math-by-intuition/", "Math by Intuition"),
               ("art-is-phenomenal/", "Art is phenomenal!"),
               ("what-is-color/", "What is color?"),
               ("about/", "About me")]

artMenu :: [(String, String)]
artMenu =
  [("photography/", "Photography"),
   ("drawings/", "Drawings"),
   ("design/", "Design"),
   ("cg/", "Computer Graphics")]

main :: IO ()
main = do
  science_md <- T.readFile "content/science/what-is-color.md"
  art_md <- T.readFile "content/science/art-is-phenomenal.md"
  math_md <- T.readFile "content/science/math-by-intuition.md"
  design_md <- T.readFile "content/art/design.md"
  drawings_md <- T.readFile "content/art/drawings.md"
  cg_md <- T.readFile "content/art/cg.md"
  
  body <- return $ homeBody
  renderToFile (homepageDir </> "index.html") (assembleDocument "Bernhard Lang | Achromatic" "resources/home_style.css" "resources/favicon.svg" body)

  right <- case MM.parse "content/science/what-is-color.md" science_md of
    Left bundle -> return . p_ $ "Couldn't load file"
    Right r -> return . MM.render $ r
  
  scB <- return (scienceBody "../" scienceMenu artMenu right)
  renderToFile (homepageDir </> "what-is-color/index.html") (assembleDocument "What is color? | Bernhard Lang" "../resources/sc_style.css" "../resources/favicon.svg" scB)

  artPhenomenal <- case MM.parse "content/science/art-is-phenomenal.md" art_md of
    Left bundle -> return . p_ $ "Couldn't load file"
    Right r -> return . MM.render $ r
  
  scB <- return (scienceBody "../" scienceMenu artMenu artPhenomenal)
  renderToFile (homepageDir </> "art-is-phenomenal/index.html") (assembleDocument "Art is Phenomenal! | Bernhard Lang" "../resources/sc_style.css" "../resources/favicon.svg" scB)

  mathIntuition <- case MM.parse "content/science/math-by-intuition" math_md of
    Left bundle -> return . p_ $ "Couldn't load file"
    Right r -> return . MM.render $ r
  
  scB <- return (scienceBody "../" scienceMenu artMenu mathIntuition)
  renderToFile (homepageDir </> "math-by-intuition/index.html") (assembleDocument "Math by Intuition | Bernhard Lang" "../resources/sc_style.css" "../resources/favicon.svg" scB)

  aboutSite <- return (scienceBody "../" scienceMenu artMenu aboutBody)
  renderToFile (homepageDir </> "about/index.html") (assembleDocument "About Bernhard Lang | fullyAchromatic" "../resources/sc_style.css" "../resources/favicon.svg" aboutSite)

  aboutSiteDE <- return (scienceBody "../../" scienceMenu artMenu aboutBodyDE)
  renderToFile (homepageDir </> "de/about/index.html") (assembleDocument "Computer+Grafik Bernhard Lang" "../../resources/sc_style.css" "../../resources/favicon.svg" aboutSiteDE)

  assembleGallery scienceMenu artMenu homepageDir "photography/index.html" "../" galleryPaths itchioWidgets

  design <- case MM.parse "content/art/design.md" design_md of
    Left bundle -> return . p_ $ "Couldn't load file"
    Right r -> return . MM.render $ r
  
  designBody <- return (artBody "../" scienceMenu artMenu design)
  renderToFile (homepageDir </> "design/index.html") (assembleDocument "Design | fullyAchromatic" "../resources/art_style.css" "../resources/favicon.svg" designBody)

  drawings <- case MM.parse "content/art/drawings.md" drawings_md of
    Left bundle -> return . p_ $ "Couldn't load file"
    Right r -> return . MM.render $ r
  
  drawingsBody <- return (artBody "../" scienceMenu artMenu drawings)
  renderToFile (homepageDir </> "drawings/index.html") (assembleDocument "Drawings | fullyAchromatic" "../resources/art_style.css" "../resources/favicon.svg" drawingsBody)
  
  cg <- case MM.parse "content/art/cg.md" cg_md of
    Left bundle -> return . p_ $ "Couldn't load file"
    Right r -> return . MM.render $ r
  
  cgBody <- return (artBody "../" scienceMenu artMenu cg)
  renderToFile (homepageDir </> "cg/index.html") (assembleDocument "Computer Graphics | fullyAchromatic" "../resources/art_style.css" "../resources/favicon.svg" cgBody)
  
  TL.writeFile (homepageDir </> "resources/home_style.css") (render assembleHomeCSS)
  TL.writeFile (homepageDir </> "resources/sc_style.css") (render assembleScienceCSS)
  TL.writeFile (homepageDir </> "resources/art_style.css") (render assembleArtCSS)
  
  gestaltPackage
