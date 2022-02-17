{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Markdown
  ( insertCanvas
  ) where

import Lucid
import Text.URI
import qualified Text.MMark.Extension as Ext

imgToCanvas :: (Ext.Inline -> Html ()) -> Ext.Inline -> Html ()
imgToCanvas renderFn (Ext.Image descr uri title)
  | uriScheme uri == Some "canvas" = canvas_ [id_ . getId . uriPath $ uri] ""
  | otherwise = renderFn (Ext.Image descr uri title)
  where getId (Some path) = path
        getId Nothing = ""
imgToCanvas renderFn elem = renderFn elem

insertCanvas :: Ext.Extension
insertCanvas = Ext.inlineRender imgToCanvas


