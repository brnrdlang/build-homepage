{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lib
    ( assembleDocument,
      homeBody,
      scienceBody,
      artBody,
      assembleGallery
    ) where

import Lucid

import Data.Text(Text, pack)
import Control.Monad

import System.Directory(listDirectory)
import System.FilePath((</>))

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
  div_ [class_ "science"] $ do
    h1_ "Bernhard Lang"
    div_ $ img_ [src_ "resources/bl_500.png"]
    p_ [style_ "text-align:left"] scienceText
--    h2_ . a_ [href_ "science.html"] $ "The note collection"
--    p_ [style_ "text-align:left"] blogText
    h2_ "Curriculum Vitae"
    table_ (tr_ (td_ [class_ "dateRow"] "2018 - Now"
              <> td_ "Phd student, University of Tübingen")
         <> tr_ (td_ [class_ "dateRow"] "2015 - 2017"
              <> td_ "M.Sc. in Neural Information Processing, University of Tübingen")
         <> tr_ (td_ [class_ "dateRow"] "2012 - 2015"
              <> td_ "B.Sc. in Computational Molecular Biology, Saarland University"))
  div_ [class_ "art"] $ do
    h1_ "fullyAchromatic"
--    p_ "Photography, drawing, computer graphics, music, animation, games."
    div_ . a_ [href_ "photography/"] $ (img_ [src_ "resources/salamander_wm.jpg", style_ "width: 12em;margin-right: 1em;margin-bottom:3em;"]
            <> img_ [src_ "resources/wm_8-August.jpg", style_ "width: 9em;"]
            <> br_ []
            <> img_ [src_ "resources/Bläuling_schmetterling_wm.jpg", style_ "width: 9em;margin-right: 1em;margin-top:-2em;"]
            <> img_ [src_ "resources/3-März.JPG", style_ "width: 12em;"])
    div_ . a_ [href_ "design/"] $ (img_ [src_ "resources/fonts.svg", style_ "width=100%;max-width: 22.5em"])

scienceMenu :: FilePath -> Html ()
scienceMenu relHomePath = do
  div_ [class_ "scienceMenu"] $ do
    a_ [href_ . pack $ relHomePath] $ img_ [src_ . pack $ relHomePath </> "resources/bl_500.png", style_ "height:4em;"]
    
artMenu :: FilePath -> Html ()
artMenu relHomePath = do
  div_ [class_ "artMenu"] $ do
    a_ [href_ . pack $ relHomePath] $ img_ [src_ . pack $ relHomePath </> "resources/achromatic-logo.png", style_ "height:4em;"]

scienceBody :: FilePath -> Html () -> Html ()
scienceBody relHomePath content = do
--  scienceMenu
  div_ [class_ "scienceContent"] content
  artMenu relHomePath

artBody :: FilePath -> Html () -> Html ()
artBody relHomePath content = do
  scienceMenu relHomePath
  div_ [class_ "artContent"] content
--  artMenu

assembleGallery :: FilePath -> FilePath -> FilePath -> [FilePath] -> [(Html (), Html ())] -> IO ()
assembleGallery homePath outPath relHomePath gpaths htmlElements = do
  img_files <- mapM listDirectory (map (\p -> homePath </> p) gpaths)
  img_paths <- return $ zipWith appendPaths gpaths img_files
  
  galleries <- return $ zipWith (\a (h, b) -> div_ [class_ "gallery"] $ (h1_ h) <> a <> b) (map (assembleGallery' . toText) img_paths) htmlElements
  gallery_html <- return $ foldl1 (<>) galleries
  body <- return $ artBody relHomePath gallery_html
  renderToFile (homePath </> outPath) $ assembleDocument "Photography | fullyAchromatic" (pack $ relHomePath </> "resources/art_style.css") body
  where toText = map pack
        appendPaths = \dirPath fpaths -> map (\fpath -> relHomePath </> dirPath </> fpath) fpaths

assembleGallery' :: [Text] -> Html ()
assembleGallery' [] = mempty

assembleGallery' (imgpath:paths) = do
  div_ [class_ "galleryElement"] $ img_ [src_ imgpath]
  assembleGallery' paths
