{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Blog.Widgets.StreamOfConsciousness.DeliciousPosts ( start_delicious ) where

import Blog.Widgets.StreamOfConsciousness.Thought
import Blog.Widgets.StreamOfConsciousness.Controller
import Blog.BackEnd.HttpPoller

import Text.XML.HXT.Arrow
import Network.HTTP
import Network.URI ( parseURI )
import Data.Maybe ( fromJust )

delicious_period :: Int
delicious_period = 360 * 10^6

start_delicious :: SoCController -> String -> IO Worker
start_delicious socc user = do { let req = Request ( fromJust . parseURI $ "http://feeds.delicious.com/rss/" ++ user ) GET [] ""
                               ; p <- start_poller "DeliciousPosts" req (handle_posts socc) delicious_period
                               ; return $ Worker socc p }

handle_posts :: SoCController -> String -> IO ()
handle_posts socc body = do { posts <- runX ( readString parse_opts body >>> getItems )
                           ; commit socc posts }

parse_opts = [(a_validate, v_0), (a_check_namespaces,v_1)]
                                
atElemQName qn = deep (isElem >>> hasQName qn)
text = getChildren >>> getText
textOf qn = atElemQName qn >>> text

rdf_uri :: String
rdf_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

rdf_RDF :: QName
rdf_RDF = mkQName "rdf" "RDF" rdf_uri

rss_uri :: String
rss_uri = "http://purl.org/rss/1.0/"

rss_item :: QName
rss_item = mkQName "rss" "item" rss_uri

rss_title :: QName
rss_title = mkQName "rss" "title" rss_uri

rss_link :: QName
rss_link = mkQName "rss" "link" rss_uri

dc_uri :: String
dc_uri = "http://purl.org/dc/elements/1.1/"

dc_date :: QName 
dc_date = mkQName "dc" "date" dc_uri

getItem = atElemQName rss_item >>>
          proc i -> do
            t <- textOf rss_title -< i
            u <- textOf rss_link -< i
            d <- textOf dc_date -< i
            returnA -< Thought Delicious d u t

getItems = atElemQName rdf_RDF >>>
           proc r -> do
             items <- getItem -< r
             returnA -< items
