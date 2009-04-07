module Blog.FrontEnd.ContentAtoms ( heading, footer, divid, _a, _at
                                  , stylesheet, base_url ) where

import Text.XHtml.Strict
import qualified Blog.Constants as C
import qualified Blog.FrontEnd.Views as V

heading :: Html
heading = divid "header" ( (h1 $ _at (V.url $ V.All Nothing) C.blog_title)
                           +++
                           (h2 $ stringToHtml C.blog_tagline) )

footer :: Html
footer = ((divid "footer") . concatHtml $ [  p ! [ theclass "copyright" ] << primHtml C.license_xhtml
                                         , p ! [ theclass "generator" ] << generator_tagline ])
         +++ google_analytics_block
    where
      generator_tagline =  concatHtml [ stringToHtml "Running "
                                      , _at C.generator_uri C.generator_name
                                      , stringToHtml " version "
                                      , stringToHtml C.generator_version ]

google_analytics_block :: Html
google_analytics_block = case C.google_analytics_identifier of
                           Nothing ->
                               noHtml
                           Just ua_code ->
                             primHtml $ "<script type=\"text/javascript\">\n"
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

divid :: String -> Html -> Html
divid label h = thediv ! [ identifier label ] << h

_a :: String -> Html -> Html
_a s h = toHtml (hotlink s h)

_at :: String -> String -> Html
_at s t = toHtml (hotlink s (stringToHtml t))

stylesheet :: Html
stylesheet = thelink ! [ href C.stylesheet_url, rel "stylesheet",  thetype "text/css" ] << noHtml

base_url :: Html
base_url = thebase ! [ href C.base_url ]