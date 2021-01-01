{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lucid
import Clay
import Lib
import CSS

import qualified Text.MMark as MM
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

main :: IO ()
main = do
  science_md <- T.readFile "content/science/free-academy.md"
--  art_md <- T.readFile "content/art.md"
  
  right <- case MM.parse "content/science/free-academy.md" science_md of
    Left bundle -> return . p_ $ "Couldn't load file"
    Right r -> return . MM.render $ r
  
  dir <- return "brnrdlang.github.io/"
  body <- return $ homeBody
  renderToFile (dir ++ "index.html") (assembleDocument "Bernhard Lang | Achromatic" "resources/style.css" body)
  
  scB <- return . scienceBody (a_ [href_ "index.html"] "HOME") $ right
  renderToFile (dir ++ "science.html") (assembleDocument "Almanach" "resources/style.css" scB)

  TL.writeFile (dir ++ "resources/style.css") (render assembleCSS)
