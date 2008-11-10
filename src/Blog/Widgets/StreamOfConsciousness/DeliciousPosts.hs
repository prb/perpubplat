module Blog.Widgets.StreamOfConsciousness.DeliciousPosts ( start_delicious ) where

import Blog.Widgets.StreamOfConsciousness.Thought
import Blog.Widgets.StreamOfConsciousness.Controller
import Blog.Widgets.StreamOfConsciousness.RssUtilities (fromRSS2)
import Blog.BackEnd.HttpPoller

import qualified Blog.Widgets.StreamOfConsciousness.XmlUtilities as XU

import Network.HTTP
import Network.URI ( parseURI )
import Data.Maybe ( fromJust )

delicious_period :: Int
delicious_period = 360 * 10^6

start_delicious :: SoCController -> String -> IO Worker
start_delicious socc user = do { let req = Request ( fromJust . parseURI $ "http://feeds.delicious.com/v2/rss/" ++ user ) GET [] ""
                               ; p <- start_poller "DeliciousPosts" req (handle_posts socc) delicious_period
                               ; return $ Worker socc p }

handle_posts :: SoCController -> String -> IO ()
handle_posts socc = (commit socc) . import qualified Blog.Widgets.StreamOfConsciousness.XmlUtilities as XU
map unescape . (fromRSS2 Delicious)

unescape :: Thought -> Thought
unescape t = case desc t of
               Nothing -> t
               Just s -> t { desc = Just . XU.substituteEntities $ s }
