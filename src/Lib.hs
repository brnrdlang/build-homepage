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

import About(encryptLink, decryptScript)

assembleDocument :: Html () -> Text -> Text -> Html () -> Html ()
assembleDocument title csspath faviconpath body = doctypehtml_ $ do
  head_ $ do
    title_ title
    meta_ [charset_ "utf-8"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ csspath]
    link_ [rel_ "icon", href_ faviconpath]
  body_ body

scienceText :: Html ()
scienceText = 
  "I am a scientist by training and an artist by calling. \
  \My goals are to explore visual perception through art and information technology \
  \and to develop new, exciting ways of storytelling. \
  \I use my skills in programming, my passion for drawing and photographing, as well as my knowledge about human vision towards these goals."
  
adText :: Html ()
adText =
  "I am open to freelance work. If you are interested in working with me, please contact me at "
  <> encryptLink "#" "fullyachromatic@gmail.com"
  <> " or find out more "
  <> a_ [href_ "about/"] "here"
  <> "."
  <> decryptScript "./"

blogText :: Html ()
blogText =
  "Here I provide you with articles about topics I am interested in."

cg_canvas_js :: Html ()
cg_canvas_js =
  toHtmlRaw "import init, { WebGlCanvas } from './resources/js/gestalt.js';\
  \ \
  \async function run() {\
  \  await init();\
  \ \
  \  const wgc = WebGlCanvas.new(\"cg_canvas\");\
  \ \
  \  const renderLoop = time => {\
  \    wgc.render(time);\
  \ \
  \    requestAnimationFrame(renderLoop);\
  \  };\
  \ \
  \  requestAnimationFrame(renderLoop);\
  \}\
  \ \
  \run();"


homeBody :: Html ()
homeBody = do
  div_ [class_ "science"] $ do
    h1_ "Bernhard Lang"
    div_ $ img_ [src_ "resources/bl_500.png"]
    p_ scienceText
    p_ adText
--    h2_ . a_ [href_ "science.html"] $ "The note collection"
--    p_ [style_ "text-align:left"] blogText
    h2_ "Curriculum Vitae"
    table_ (tr_ (td_ [class_ "dateRow"] "2018 - 2021"
              <> td_ "Scientific researcher, University of Tübingen")
         <> tr_ (td_ [class_ "dateRow"] "2015 - 2017"
              <> td_ "M.Sc. in Neural Information Processing, University of Tübingen")
         <> tr_ (td_ [class_ "dateRow"] "2012 - 2015"
              <> td_ "B.Sc. in Computational Molecular Biology, Saarland University"))
    div_ [style_ "border:1px solid #000;margin:1em 0em"] $ do
      h2_ $ a_ [href_ "art-is-phenomenal/", style_ "text-decoration:none"] "Art is phenomenal!"
      p_ $ a_ [href_ "art-is-phenomenal/", style_ "text-decoration:none"] "... Wouldn't you agree? Doesn't matter if you're thinking of your favorite TV show, that album you've listened to twenty times on repeat, the theater play that made you puzzled afterwards or that painting that filled you with awe. While I also completely agree with this interpretation of the phrase, there is another meaning I would like to endorse ..."
    div_ [style_ "border:1px solid #000;margin:1em 0em"] $ do
      h2_ $ a_ [href_ "what-is-color/", style_ "text-decoration:none"] "What is color?"
      p_ $ a_ [href_ "what-is-color/", style_ "text-decoration:none"] "What do you see when you look around you? If you are reading this at a desk on a computer monitor, you probably have a keyboard and mouse before you, maybe there is a coffee mug next to it. The world, as we experience it, consists of objects and every object has (at least one) color ..."
    
  div_ [class_ "art"] $ do
--    p_ "Photography, drawing, computer graphics, music, animation, games."
    div_ . a_ [href_ "photography/"] $ (img_ [src_ "resources/salamander_wm.jpg", style_ "width: 12em;margin-right: 1em;margin-bottom:3em;"]
            <> img_ [src_ "resources/wm_8-August.jpg", style_ "width: 9em;"]
            <> br_ []
            <> img_ [src_ "resources/Bläuling_schmetterling_wm.jpg", style_ "width: 9em;margin-right: 1em;margin-top:-2em;"]
            <> img_ [src_ "resources/3-März.JPG", style_ "width: 12em;"])
    div_ . a_ [href_ "design/"] $ (img_ [src_ "resources/fonts.svg", style_ "width: 22.5em"])
    div_ . a_ [href_ "drawings/"] $ (img_ [src_ "resources/drawings/self-portrait.jpg", style_ "width:22em"])
    div_ . a_ [href_ "cg/"] . canvas_ [id_ "cg_canvas", style_"width:22em;height:22em"] $ ""
    script_ [type_ "module"] $ cg_canvas_js
    h1_ "fullyAchromatic"

scienceMenu :: FilePath -> [(String, String)] -> [(String, String)] -> Html ()
scienceMenu relHomePath scLinks artLinks = do
  div_ [class_ "scienceMenu"] $ do
    a_ [href_ . pack $ relHomePath] $ img_ [src_ . pack $ relHomePath </> "resources/bl_500.png", style_ "height:4em;"]
    forM_ scLinks (buildLink relHomePath "scienceLink")
    div_ [style_ "border-bottom: 1px solid #000"] ""
    forM_ artLinks (buildLink relHomePath "artLink")
  where
    buildLink = \rel -> \cl -> \(url, name) -> div_ [class_ cl] . a_ [href_ . pack $ rel </> url] . toHtml $ name
    
artMenu :: FilePath -> [(String, String)] -> [(String, String)] -> Html ()
artMenu relHomePath scLinks artLinks = do
  div_ [class_ "artMenu"] $ do
    a_ [href_ . pack $ relHomePath] $ img_ [src_ . pack $ relHomePath </> "resources/achromatic-logo.png", style_ "height:4em;"]
    forM_ scLinks (buildLink relHomePath "scienceLink")
    div_ [style_ "border-bottom: 1px solid #fff"] ""
    forM_ artLinks (buildLink relHomePath "artLink")
  where
    buildLink = \rel -> \cl -> \(url, name) -> div_ [class_ cl] . a_ [href_ . pack $ rel </> url] . toHtml $ name

scienceBody :: FilePath -> [(String, String)] -> [(String, String)] -> Html () -> Html ()
scienceBody relHomePath scLinks artLinks content = do
--  scienceMenu
  div_ [class_ "scienceContent"] content
  artMenu relHomePath scLinks artLinks

artBody :: FilePath -> [(String, String)] -> [(String, String)] -> Html () -> Html ()
artBody relHomePath scLinks artLinks content = do
  scienceMenu relHomePath scLinks artLinks
  div_ [class_ "artContent"] content
--  artMenu

assembleGallery :: [(String, String)] -> [(String, String)] -> FilePath -> FilePath -> FilePath -> [FilePath] -> [(Html (), Html ())] -> IO ()
assembleGallery scLinks artLinks homePath outPath relHomePath gpaths htmlElements = do
  img_files <- mapM listDirectory (map (\p -> homePath </> p) gpaths)
  img_paths <- return $ zipWith appendPaths gpaths img_files
  
  galleries <- return $ zipWith (\a (h, b) -> div_ [class_ "gallery"] $ (h1_ h) <> a <> b) (map (assembleGallery' . toText) img_paths) htmlElements
  gallery_html <- return $ foldl1 (<>) galleries
  body <- return $ artBody relHomePath scLinks artLinks gallery_html
  renderToFile (homePath </> outPath) $ assembleDocument "Photography | fullyAchromatic" (pack $ relHomePath </> "resources/art_style.css") (pack $ relHomePath </> "resources/favicon.svg") body
  where toText = map pack
        appendPaths = \dirPath fpaths -> map (\fpath -> relHomePath </> dirPath </> fpath) fpaths

assembleGallery' :: [Text] -> Html ()
assembleGallery' [] = mempty

assembleGallery' (imgpath:paths) = do
  div_ [class_ "galleryElement"] $ img_ [src_ imgpath]
  assembleGallery' paths
