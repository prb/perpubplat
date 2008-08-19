{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Blog.Widgets.StreamOfConsciousness.GoogleReader ( start_google_reader, google_user ) where

import Blog.Widgets.StreamOfConsciousness.Thought
import Blog.Widgets.StreamOfConsciousness.Controller
import Blog.BackEnd.HttpPoller

import Text.XML.HXT.Arrow
import Network.HTTP
import Network.URI ( parseURI )
import Data.Maybe ( fromJust )

import qualified Codec.Binary.UTF8.String as UTF8


google_reader_period :: Int
google_reader_period = 240 * 10^6

google_user :: String
google_user = "14627107182419169041"

start_google_reader :: SoCController -> String -> IO Worker
start_google_reader socc user = do { let req = Request ( fromJust . parseURI $ "http://www.google.com/reader/public/atom/user/" ++ user ++ "/state/com.google/broadcast" ) GET [] ""
                                   ; p <- start_poller "GoogleReaderSharedItems" req (handle_posts socc) google_reader_period
                                   ; return $ Worker socc p }

handle_posts :: SoCController -> String -> IO ()
handle_posts socc body = do { posts <- runX ( readString parse_opts ( UTF8.decodeString body ) >>> getEntries )
                           ; commit socc posts }

parse_opts = [(a_validate, v_0), (a_check_namespaces,v_1)]
                                
atElemQName qn = deep (isElem >>> hasQName qn)
childElemQName qn = getChildren >>> isElem >>> hasQName qn
text = getChildren >>> getText
textOf qn = childElemQName qn >>> text

atom_uri :: String
atom_uri = "http://www.w3.org/2005/Atom"

atom_entry :: QName
atom_entry = mkQName "atom" "entry" atom_uri

atom_title :: QName
atom_title = mkQName "atom" "title" atom_uri

atom_updated :: QName
atom_updated = mkQName "atom" "updated" atom_uri

atom_link :: QName
atom_link = mkQName "atom" "link" atom_uri

atom_feed :: QName
atom_feed = mkQName "atom" "feed" atom_uri

getEntry = atElemQName atom_entry >>>
          proc i -> do
            t <- textOf atom_title -< i
            l <- childElemQName atom_link >>> hasAttrValue "rel" ((==) "alternate") -< i
            u <- getAttrValue "href" -< l
            d <- textOf atom_updated -< i
            returnA -< Thought GoogleReader d u t

getEntries = atElemQName atom_feed >>>
           proc r -> do
             entries <- getEntry -< r
             returnA -< entries
