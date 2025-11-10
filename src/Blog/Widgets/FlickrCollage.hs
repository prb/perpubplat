module Blog.Widgets.FlickrCollage where

import Blog.Widgets.JsonUtilities
import Blog.FrontEnd.ContentAtoms
import Blog.BackEnd.HttpPoller

import qualified System.Log.Logger as L

import Random

import Network.HTTP.Client
import qualified Data.ByteString.Char8 as BS
import Text.JSON
import Data.Maybe
import Control.Concurrent.MVar
import Data.Char
import Data.List

import Text.XHtml.Strict

log_handle :: String 
log_handle = "FlickrCollage"

build_collage :: FlickrPhotos -> IO Html
build_collage fp = do { images <- get_photos fp
                      ; return $ ( divid "flickr_badge_uber_wrapper")
                        . ( divid "flickr_badge_wrapper" )
                        . concatHtml $ [ concatHtml . (map (to_xhtml $ flickr_user fp)) $ images
                                       , flickr_link ] }

flickr_link :: Html
flickr_link = primHtml $ "<a href=\"http://www.flickr.com\" id=\"flickr_www\">"
              ++ "www.<strong style=\"color:#3993ff\">flick"
              ++ "<span style=\"color:#ff1c92\">r</span></strong>.com</a>"

flickr_service_url :: String
flickr_service_url = "http://api.flickr.com/services/rest/"

flickr_view_url :: String -> String
flickr_view_url user_id = "http://www.flickr.com/photos/" ++ user_id ++ "/"

display_count :: Int
display_count = 10

photo_count :: Int
photo_count = 500

polling_frequency :: Int
polling_frequency = 15 * 60 * 10^6 -- 15 minutes

data FlickrPhotos = FlickrPhotos { box :: MVar [FlickrPhoto]
                                 , poller :: HttpPoller
                                 , flickr_user :: String }

data FlickrPhoto = FlickrPhoto { photo_id :: String
                               , owner :: String
                               , secret :: String
                               , server :: String
                               , farm :: Int
                               , photo_title :: String }
                 deriving ( Show, Ord, Eq )

boot :: String -> String -> IO FlickrPhotos
boot user_id api_key =
    do { box <- newMVar []
       ; p <- start_poller "FlickrCollage" (flickr_people_getPublicPhotos_req user_id api_key)
                 (handle_flickr_response box) polling_frequency
       ; return $ FlickrPhotos box p user_id}

handle_flickr_response :: MVar [FlickrPhoto] -> String -> IO ()
handle_flickr_response fc body = case parse_utf8_json body of 
                                   Right v ->
                                       do { let photos = map to_photo ( una $ v </> "photos" </> "photo" )
                                          ; L.infoM log_handle $ "Retrieved " ++ (show . length $ photos) ++ " photos."
                                          ; put_photos fc photos }
                                   Left err ->
                                       L.errorM log_handle err

get_photos :: FlickrPhotos -> IO [FlickrPhoto]
get_photos fp = do { photos <- readMVar $ box fp
                   ; case photos of
                       [] ->
                           return []
                       _ ->
                           do { let c = min (length photos) display_count
                              ; idxs <- n_different_random (0,(length photos)-1) c []
                              ; return $ map ((!!) photos) idxs } }

put_photos :: MVar [FlickrPhoto] -> [FlickrPhoto] -> IO ()
put_photos box photos = swapMVar box photos >> return ()

compose_uri :: String -> [(String,String)] -> URI
compose_uri b a = fromJust . parseURI $ b ++ "?" ++ (conc a)
    where
      to_nvp = \(x,y) -> x ++ "=" ++ (urlEncode y)
      conc = concat . (intersperse "&") . (map to_nvp)

flickr_people_getPublicPhotos_req :: String -> String -> Request
flickr_people_getPublicPhotos_req user api_key =
    let url = compose_uri flickr_service_url [ ("api_key", api_key)
                                             , ("format","json")
                                             , ("nojsoncallback","1")
                                             , ("method","flickr.people.getPublicPhotos")
                                             , ("per_page",show photo_count)
                                             , ("user_id",user) ]
    in (fromJust $ parseRequest url)
       { requestHeaders = [(BS.pack "Accept-Charset", BS.pack "utf-8")] }

to_xhtml :: String -> FlickrPhoto -> Html
to_xhtml user fp = _a (photo_page_url user fp) ( image ! [ src (image_url fp)
                                               , theclass "flickr_badge_image"
                                               , alt (photo_title fp) ] )

to_photo :: JSValue -> FlickrPhoto
to_photo m = FlickrPhoto { photo_id = uns $ m </> "id"
                         , owner = uns $ m </> "owner"
                         , secret = uns $ m </> "secret"
                         , server = uns $ m </> "server"
                         , photo_title = uns $ m </> "title"
                         , farm = unn $ m </> "farm" }

photo_page_url :: String -> FlickrPhoto -> String
photo_page_url user fp = (flickr_view_url user) ++ (photo_id fp)

image_url :: FlickrPhoto -> String
image_url fp = "http://farm" ++ (show $ farm fp) ++ ".static.flickr.com/" ++ (server fp)
               ++ "/" ++ (photo_id fp) ++ "_" ++ (secret fp) ++ "_t.jpg"

n_different_random :: (Int,Int) -> Int -> [Int] -> IO [Int]
n_different_random (l,h) n ns = do { x <- getStdRandom $ randomR (l,h)
                                   ; if (x `elem` ns) then
                                         n_different_random (l,h) n ns
                                     else
                                         if (n == 1) then
                                             return (x:ns)
                                         else 
                                             n_different_random (l,h) (n-1) (x:ns) }
