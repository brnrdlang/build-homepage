{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module About
    ( aboutBody,
      aboutBodyDE,
      encryptLink,
      decryptScript
    ) where

import Lucid

import Control.Monad
import Data.Bits(xor)
import Numeric (showHex)
import Data.Text(Text, pack)
import Text.Printf
import System.FilePath((</>))

aboutBody :: Html ()
aboutBody = do
  h1_ "Bernhard Lang"
  p_ "I am a scientific researcher and visual artist. I started in the sciences, but I am now exploring the opportunity for an alternative to a classic academic carrier."
  p_ "I want to explore visual perception through an artistic lens. Furthermore I want to make scientific discoveries of the field accessible to everyone, through the use of digital media."
  p_ "Over the years of my academic and scientific education, I aquired expertise in the following fields:"
  ul_ (li_ "Data science: analysis and visualization"
    <> li_ "3D graphics and illustrations"
    <> li_ "Cognitive science and visual perception")
  p_ "Next to my personal publicistic and artistic projects I offer consulting services for all of these topics. If you want to work with me, please contact me on one of the channels mentioned below."
  h2_ "Curriculum Vitae"
  table_ (tr_ (td_ [class_ "dateRow"] "2022"
              <> td_ "Freelancer, visual arts and data science")
         <> tr_ (td_ [class_ "dateRow"] "2018 - 2021"
              <> td_ "Scientific researcher, University of Tübingen")
         <> tr_ (td_ [class_ "dateRow"] "2015 - 2017"
              <> td_ "M.Sc. in Neural Information Processing, University of Tübingen")
         <> tr_ (td_ [class_ "dateRow"] "2012 - 2015"
              <> td_ "B.Sc. in Computational Molecular Biology, Saarland University"))
  h2_ "Get in contact"
  p_ contactInfo
  h2_ "Socials"
  p_ socials
  p_ ("Support or commission me on "
   <> a_ [href_ "https://www.ko-fi.com/fullyachromatic"] "Ko-fi.com/fullyachromatic")
  p_ ("Get merch on "
   <> a_ [href_ "https://fullyachromatic.redbubble.com"] "fullyachromatic.redbubble.com")
  decryptScript "../"

encrypt :: String -> String
encrypt (x:y:xs) = (printf "%02x" $ fromEnum x) ++ (encrypt' (fromEnum x) (y:xs))
encrypt (x:[]) = (showHex . fromEnum $ x) ""
encrypt [] = ""

encryptLink :: Text -> String -> Html ()
encryptLink mode text = a_ [class_ "link-obscured", href_ mode] $ toHtml . encrypt $ text

encrypt' :: Int -> String -> String
encrypt' c (x:xs) = (printf "%02x" $ xor c xn) ++ (encrypt' c xs)
  where
    xn = fromEnum x
encrypt' c [] = ""

decryptScript :: FilePath -> Html ()
decryptScript relHomePath = script_ [src_ . pack $ relHomePath </> "resources/js/unobscure-links.js", type_ "text/javascript"] ""

contactInfo :: Html ()
contactInfo = do
  "Computer+Grafik Bernhard Lang"
  br_ []
  "Seetalhof 1"
  br_ []
  "72525 Münsingen"
  br_ []
  "Germany"
  br_ []
  br_ []
  "E-Mail: "
  encryptLink "#" "fullyachromatic@gmail.com"
  br_ []
  "Phone: "
  encryptLink "#+" "+49 175 664 2222"

contactInfoDE :: Html ()
contactInfoDE = do
  "Computer+Grafik Bernhard Lang"
  br_ []
  "Seetalhof 1"
  br_ []
  "72525 Münsingen"
  br_ []
  br_ []
  "E-Mail: "
  encryptLink "#" "fullyachromatic@gmail.com"
  br_ []
  "Mobil: "
  encryptLink "#+" "+49 175 664 2222"

socials :: Html ()
socials = do
  "Twitter: "
  a_ [href_ "https://www.twitter.com/brnrdlang"] "@brnrdlang"
  " / "
  a_ [href_ "https://www.twitter.com/fullyachromatic"] "@fullyachromatic"
  br_ []
  "Github: "
  a_ [href_ "https://www.github.com/brnrdlang"] "@brnrdlang"
  br_ []
  "Instagram: "
  a_ [href_ "https://www.instagram.com/fullyachromatic"] "@fullyachromatic"

aboutBodyDE :: Html ()
aboutBodyDE = do
  h1_ "Bernhard Lang"
  p_ "Meine bisherige Laufbahn folgte von einem Bioinformatikstudium über Neurowissenschaften zu einer Promotionstelle über das menschliche Sehen einer klassischen Karriere in der akademischen Forschung. Zeiten der Isolation während der Corona-Pandemie haben mich meine Interessen, Stärken und Schwächen überdenken lassen und führten dazu eine Alternative zur Karriere an der Universität zu verfolgen."
  p_ "Nun beschäftige ich mich mit visueller Wahrnehmung durch eine künstlerische Linse gesehen. Ich habe es mir außerdem zum Ziel gesetzt, moderne Forschungsergebnisse des Gebietes durch den Einsatz digitaler Medien für eine breites Publikum zugänglich zu machen."
  p_ "Im Verlauf meiner wissenschaftlichen und akademischen Ausbildung habe ich mir Expertise in den folgenden Gebieten erarbeitet:"
  ul_ (li_ "Datenanalyse und Visualisierung"
    <> li_ "3D Grafik und Illustration"
    <> li_ "Kognitionswissenschaft und visuelle Wahrnehmung")
  p_ "Neben meinen eigenen publizistischen und künstlerischen Projekten biete ich Beratung in all diesen Gebieten an. Nehmen Sie dazu gerne Kontakt über die unten gegebenen Kontaktdaten oder Plattformen mit mir auf."
  h2_ "Lebenslauf"
  table_ (tr_ (td_ [class_ "dateRow"] "2022"
              <> td_ "Freelancer, Data Science und Grafikdesign")
         <> tr_ (td_ [class_ "dateRow"] "2018 - 2021"
              <> td_ "Wissenschaftlicher Mitarbeiter, Eberhard Karls Universität Tübingen")
         <> tr_ (td_ [class_ "dateRow"] "2015 - 2017"
              <> td_ "M.Sc. in Neuronale Informationsverarbeitung, Eberhard Karls Universität Tübingen")
         <> tr_ (td_ [class_ "dateRow"] "2012 - 2015"
              <> td_ "B.Sc. in Bioinformatik, Universität des Saarlandes"))
  h2_ "Kontaktinformation"
  p_ contactInfoDE
  h2_ "Socials"
  p_ socials
  p_ ("Unterstütze mich auf "
   <> a_ [href_ "https://www.ko-fi.com/fullyachromatic"] "Ko-fi.com/fullyachromatic")
  p_ ("Finde Merch auf "
   <> a_ [href_ "https://fullyachromatic.redbubble.com"] "fullyachromatic.redbubble.com")
  decryptScript "../../"

