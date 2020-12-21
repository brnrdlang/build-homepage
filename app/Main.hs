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
--  science_md <- T.readFile "content/science.md"
--  art_md <- T.readFile "content/art.md"
  
--  right <- case MM.parse "content/art.md" art_md of
--    Left bundle -> return . p_ $ "Couldn't load art gallery xD"
--    Right r -> return . MM.render $ r
  
  body <- return $ homeBody
  renderToFile "html/index.html" (assembleDocument "Bernhard Lang | Achromatic" "resources/style.css" body)
  TL.writeFile "html/resources/style.css" (render assembleCSS)
