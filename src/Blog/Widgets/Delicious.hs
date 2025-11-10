{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Blog.Widgets.Delicious where

import qualified Blog.Model.Entry as B
import qualified Utilities as Ut
import Blog.Widgets.JsonUtilities
import Data.Aeson (Value(..))
import qualified Data.Vector as V

import qualified System.Log.Logger as L

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Digest.Pure.MD5
import Data.List ( intersperse )

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent ( forkIO, myThreadId, ThreadId, threadDelay, killThread )
import Control.Exception (catch)
import Data.ByteString.Lazy.Char8 ( pack )
import qualified Data.Map as M
import qualified Data.IntSet as S
import Data.Maybe
import qualified System.Posix as SP
import qualified Data.Time.Clock.POSIX as DTCP

import Blog.FrontEnd.ContentAtoms
import Lucid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

log_handle :: String
log_handle = "DeliciousLinks"

get_chrome :: Controller -> B.Model -> B.Item -> IO (Html ())
get_chrome dc _ i = do { (_,h) <- get_record dc $ B.internal_id i
                       ; return $ h }

to_html :: DeliciousRecord -> B.Model -> B.Item -> Html ()
to_html dr m i = div_ [class_ "delicious_chrome"] $ do
    if dr == empty_record
        then mempty
        else p_ $ do
            _at (url_for_bookmark dr) "Bookmarked"
            toHtml $ " by " ++ (show $ total_posts dr)
                  ++ (if total_posts dr > 1 then " people. " else " person. ")
            if 0 /= (length $ top_tags dr)
                then mconcat $ intersperse (toHtml (", " :: String)) $ map tag_link_with_count (top_tags dr)
                else mempty
    p_ $ do
        _at (post_to_delicious plink title) "Bookmark"
        toHtml (" this post on " :: String)
        delicious_img
        _at "http://del.icio.us" "del.icio.us"
        toHtml ("." :: String)
  where
    plink = B.permalink m i
    title = B.title i

tag_link_with_count :: (String, Int) -> Html ()
tag_link_with_count (t,c) = do
    _at (link_for_tag t) t
    toHtml $ " (" ++ (show c) ++ ")"

link_for_tag :: String -> String
link_for_tag t = "http://del.icio.us/popular/" ++ (urlEncode t)

post_to_delicious :: String -> String -> String
post_to_delicious url title = "http://del.icio.us/post?v=4&url=" ++ (urlEncode url) ++ "&title="
                              ++ (urlEncode $ strip title)
strip = id

url_for_bookmark :: DeliciousRecord -> String
url_for_bookmark dr = "http://del.icio.us/url/" ++ hash dr

delicious_img :: Html ()
delicious_img = img_ [ src_ "http://images.del.icio.us/static/img/delicious.small.gif"
                     , alt_ "del.icio.us logo"
                     , class_ "delicious_logo_badge" ]

data DeliciousRecord = DeliciousRecord { hash :: String
                                       , top_tags :: [(String,Int)]
                                       , total_posts :: Int
                                       , url :: String }
                       deriving ( Show, Ord, Eq )

data Scheduler = Scheduler { s_request_channel :: Chan SRequest
                           , s_tid :: ThreadId
                           , next_actions :: [(DTCP.POSIXTime,Int)]
                           , d_con :: Controller
                           , active :: S.IntSet }

data SRequest = UpdateModel { model :: B.Model }
              | Trigger { go_time :: DTCP.POSIXTime }

boot_s :: Controller -> B.Model -> IO Scheduler
boot_s dc m = do { c <- newChan
                 ; e <- DTCP.getPOSIXTime
                 ; let n_a = zip 
                             (map (\x -> e+(2*(fromIntegral x))) [1..(length $ B.all_posts m)])
                             (map B.internal_id $ B.all_posts m)
                 ; t <- myThreadId
                 ; let s = Scheduler c t n_a dc S.empty
                 ; tid <- forkIO $ s_loop s m
                 ; L.infoM log_handle $ "Forked back-end scheduler as thread ID " ++ (show tid)
                 ; return $ s { s_tid = tid } }

data Controller = Controller { d_request_channel :: Chan DRequest
                             , dc_tid :: ThreadId }

type DeliciousState = M.Map Int (DeliciousRecord, Html ())

data DRequest = GetDRecord { callback :: MVar (DeliciousRecord, Html ())
                           , item_id :: Int }
              | PutDRecord { item_id :: Int
                           , record :: (DeliciousRecord, Html ())}

boot_dc :: IO Controller
boot_dc = do { c <- newChan
             ; t <- myThreadId
             ; let dc = Controller c t
             ; tid <- forkIO $ dc_loop dc M.empty
             ; L.infoM log_handle $ "Forked front-end controller as thread ID " ++ (show tid)
             ; return $ dc { dc_tid = tid } }

update_model :: Scheduler -> B.Model -> IO ()
update_model s m = writeChan ( s_request_channel s ) $ UpdateModel m

get_record :: Controller -> Int -> IO (DeliciousRecord, Html ())
get_record dc i = do { cb <- newEmptyMVar
                     ; writeChan ( d_request_channel dc ) $ GetDRecord cb i
                     ; takeMVar cb }

put_record :: Controller -> Int -> (DeliciousRecord, Html ()) -> IO ()
put_record dc i (r,h) = writeChan ( d_request_channel dc) $ PutDRecord i (r,h)

empty_record :: DeliciousRecord
empty_record = DeliciousRecord "" [] 0 ""

dc_loop :: Controller -> DeliciousState -> IO ()
dc_loop dc ds = do { req <- readChan $ d_request_channel dc
                   ; case req of
                       GetDRecord cb ii ->
                           do { putMVar cb $ M.findWithDefault (empty_record, mempty) ii ds
                              ; dc_loop dc ds }
                       PutDRecord ii (r,h) ->
                           dc_loop dc $ M.insert ii (r,h) ds
                   }

data FixedFrequencyDriver = FixedFrequencyDriver { target_scheduler :: Scheduler
                                                 , delay :: Int
                                                 , ffd_tid :: ThreadId }

start_driver :: Scheduler -> Int -> IO FixedFrequencyDriver
start_driver s i = do { t <- myThreadId
                      ; let ffd = FixedFrequencyDriver s i t
                      ; tid <- forkIO $ ffd_loop ffd
                      ; return ffd { ffd_tid = tid } }

kill_driver :: FixedFrequencyDriver -> IO ()
kill_driver = killThread . ffd_tid

ffd_loop :: FixedFrequencyDriver -> IO ()
ffd_loop ffd = do { L.debugM log_handle "Waking up worker..."
                  ; fire $ target_scheduler ffd
                  ; threadDelay $ delay ffd
                  ; ffd_loop ffd }

fire :: Scheduler -> IO ()
fire s = do { e <- DTCP.getPOSIXTime
            ; writeChan (s_request_channel s) $ Trigger e }

s_loop :: Scheduler -> B.Model -> IO ()
s_loop s m = do { req <- readChan $ s_request_channel s
                ; case req of
                    UpdateModel m' ->
                        do { s_loop s m'}
                    Trigger t ->
                        case (next_actions s) of
                          ((t0,i):ts) | t0 < t ->
                              do { t1 <- update_and_reschedule (d_con s) m i
                                 ; let next_actions' = insert (t1,i) ts
                                 ; L.debugM log_handle $ "Scheduling next poll for " ++ ( B.permalink m $ B.item_by_id m i )
                                                ++ " for " ++ ( format_posix_time t1) ++ "."
                                 ; s_loop s { next_actions = next_actions'} m }
                          _ ->
                              do { L.debugM log_handle "No next action registered; reading from request channel."
                                 ; s_loop s m }
                }

insert :: (Ord a) => a -> [a] -> [a]
insert x xs = (fst p) ++ (x:(snd p))
    where
      p = span (\y -> y < x) xs

update_and_reschedule :: Controller -> B.Model -> Int -> IO DTCP.POSIXTime
update_and_reschedule dc m i = do { let item = B.item_by_id m i
                                  ; age <- Ut.days_since $ B.created item
                                  ; mdr <- fetch_url_data $ B.permalink m item
                                  ; case mdr of
                                      Just dr ->
                                          put_record dc (B.internal_id item) (dr,to_html dr m item)
                                      Nothing ->
                                          return ()
                                  ; next_time age}

next_time :: Int -> IO DTCP.POSIXTime
next_time t = do { n <- DTCP.getPOSIXTime
                 ; return $ n + fromIntegral (60 * (t+1)) }

url_fragment :: String
url_fragment = "http://badges.del.icio.us/feeds/json/url/data?hash="

bookmarks_fragment :: String
bookmarks_fragment = "http://del.icio.us/feeds/json/"

request_for_bookmarks :: String -> Request
request_for_bookmarks user = fromJust $ parseRequest $
                                       bookmarks_fragment ++ user ++ "?raw"

request_for_url_data :: String -> Request
request_for_url_data u = fromJust $ parseRequest $
                                   url_fragment ++ (show . md5 . pack $ u )

fetch_url_data :: String -> IO (Maybe DeliciousRecord)
fetch_url_data url = do { L.infoM log_handle $ "Loading data for " ++ url
                        ; mgr <- newManager tlsManagerSettings
                        ; res <- (Right <$> httpLbs (request_for_url_data url) mgr)
                                 `catch` (\(e :: HttpException) -> return $ Left e)
                        ; case res of
                            Right resp | statusCode (responseStatus resp) == 200 ->
                                process_body $ LBS.unpack $ responseBody resp
                            Right resp | statusCode (responseStatus resp) `elem` [301, 302] ->
                                do { let location = lookup (BS.pack "Location") (responseHeaders resp)
                                   ; L.infoM log_handle $ "Server returned a " ++ (show $ statusCode $ responseStatus resp)
                                                  ++ " with new location " ++ (maybe "" BS.unpack location)
                                   ; return Nothing }
                            Right resp ->
                                do { let status = responseStatus resp
                                   ; L.errorM log_handle $ "Received " ++ (show $ statusCode status)
                                                ++ " response code from delicious with reason " ++ (BS.unpack $ statusMessage status)
                                   ; return Nothing }
                            Left err ->
                                do { L.errorM log_handle $ "Error connecting to " ++ (show url) ++ ": " ++ (show err)
                                   ; return Nothing }
                        }

show_rc :: (Int, Int, Int) -> String
show_rc (x,y,z) = show $ (100 * x) + (10 * y) + z

process_body :: String -> IO (Maybe DeliciousRecord)
process_body body =
    case parse_utf8_json $ unescape body  of
      Right (Array a) ->
          return $ Just (unpack_json $ V.toList a)
      Right _ ->
          do { L.errorM log_handle $ "JSON response did not contain an array at the top level."
             ; return Nothing }
      Left err ->
          do { L.errorM log_handle $ "Error parsing JSON: " ++ err
             ; return Nothing }

unpack_json :: [Value]  -> DeliciousRecord
unpack_json [] = DeliciousRecord "" [] 0 ""
unpack_json [o] = DeliciousRecord { hash = unsWithDefault "" $ o </> "hash"
                                  , top_tags = to_tag_list $ o </> "top_tags"
                                  , url = unsWithDefault "" $ o </> "url"
                                  , total_posts = unnWithDefault 0 $ o </> "total_posts" }

to_tag_list :: Value -> [(String,Int)]
to_tag_list (Array v) | V.length v == 1 =
    case V.head v of
        Object obj -> map (\(s,n) -> (s, unn n)) $ uno (Object obj)
        _ -> []
to_tag_list _ = []

unescape :: String -> String
unescape s = unescape_ [] s

unescape_ :: String -> String -> String
unescape_ s [] = reverse s
unescape_ t ('\\':'\'':ss) = unescape_ ('\'':t) ss
unescape_ t (s:ss) = unescape_ (s:t) ss

format_posix_time :: DTCP.POSIXTime -> String
format_posix_time = show . DTCP.posixSecondsToUTCTime . realToFrac