{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Blog.Widgets.StreamOfConsciousness.Identica ( start_identica ) where

import Blog.Widgets.StreamOfConsciousness.Thought
import Blog.Widgets.StreamOfConsciousness.Controller
import Blog.BackEnd.HttpPoller
import Blog.Widgets.StreamOfConsciousness.RssUtilities ( fromRSS1 )

import Network.HTTP.Client
import Data.Maybe ( fromJust )

-- import qualified Codec.Binary.UTF8.String as UTF8

identica_period :: Int
identica_period = 360 * 10^6

start_identica :: SoCController -> String -> IO Worker
start_identica socc user = do { let req = fromJust $ parseRequest $ "http://identi.ca/" ++ user ++ "/rss"
                              ; p <- start_poller "Identi.ca" req (handle_posts socc) identica_period
                              ; return $ Worker socc p }

handle_posts :: SoCController -> String -> IO ()
handle_posts socc = (commit socc) . (fromRSS1 Identica)
