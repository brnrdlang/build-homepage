{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lucid
import Clay hiding (map)
import Lib
import CSS

import qualified Text.MMark as MM
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import System.FilePath((</>))

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

main :: IO ()
main = do
  science_md <- T.readFile "content/science/Carson-2001.md"
--  art_md <- T.readFile "content/art.md"
  
  right <- case MM.parse "content/science/Carson-2001.md" science_md of
    Left bundle -> return . p_ $ "Couldn't load file"
    Right r -> return . MM.render $ r

  body <- return $ homeBody
  renderToFile (homepageDir </> "index.html") (assembleDocument "Bernhard Lang | Achromatic" "resources/home_style.css" body)
  
  scB <- return (scienceBody "./" right)
  renderToFile (homepageDir </> "science.html") (assembleDocument "Carson (2001) | Note collection" "resources/sc_style.css" scB)

  assembleGallery homepageDir "photography/index.html" "../" galleryPaths itchioWidgets

  TL.writeFile (homepageDir </> "resources/home_style.css") (render assembleHomeCSS)
  TL.writeFile (homepageDir </> "resources/sc_style.css") (render assembleScienceCSS)
  TL.writeFile (homepageDir </> "resources/art_style.css") (render assembleArtCSS)
