{-# LANGUAGE OverloadedStrings #-}
module Blog.FrontEnd.ContentAtoms ( heading, footer, divid, _a, _at
                                  , stylesheet, base_url ) where

import Lucid
import qualified Data.Text as T
import qualified Blog.Constants as C
import qualified Blog.FrontEnd.Views as V

heading :: Html ()
heading = divid "header" $ do
    h1_ $ _at (V.url $ V.All Nothing) C.blog_title
    h2_ $ toHtml C.blog_tagline

footer :: Html ()
footer = do
    divid "footer" $ do
        p_ [class_ "copyright"] $ toHtmlRaw C.license_xhtml
        p_ [class_ "generator"] generator_tagline
    google_analytics_block
  where
    generator_tagline = do
        toHtml ("Running " :: String)
        _at C.generator_uri C.generator_name
        toHtml (" version " :: String)
        toHtml C.generator_version

google_analytics_block :: Html ()
google_analytics_block = case C.google_analytics_identifier of
    Nothing -> mempty
    Just ua_code ->
        toHtmlRaw $ "<script type=\"text/javascript\">\n"
                 ++ "var gaJsHost = \"http://www.\";\n"
                 ++ "document.write(unescape(\"%3Cscript src='\""
                 ++ " + gaJsHost + \"google-analytics.com/ga.js' "
                 ++ "type='text/javascript'%3E%3C/script%3E\"));"
                 ++ "</script>\n"
                 ++ "<script type=\"text/javascript\">\n"
                 ++ "try{\n"
                 ++ "  var pageTracker = _gat._getTracker(\""
                 ++ ua_code
                 ++ "\");\n"
                 ++ "  pageTracker._trackPageview();"
                 ++ "} catch(err) {}"
                 ++ "</script>"

divid :: String -> Html () -> Html ()
divid label h = div_ [id_ (T.pack label)] h

_a :: String -> Html () -> Html ()
_a url content = a_ [href_ (T.pack url)] content

_at :: String -> String -> Html ()
_at url text = a_ [href_ (T.pack url)] (toHtml text)

stylesheet :: Html ()
stylesheet = link_ [href_ (T.pack C.stylesheet_url), rel_ "stylesheet", type_ "text/css"]

base_url :: Html ()
base_url = base_ [href_ (T.pack C.base_url)]
