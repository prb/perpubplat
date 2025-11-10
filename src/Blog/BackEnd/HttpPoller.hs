module Blog.BackEnd.HttpPoller where

import Data.Char
import Data.Maybe
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI

import qualified System.Log.Logger as L

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

import qualified Control.Exception as E
import qualified Data.Time.Clock as DTC
import Control.Concurrent ( ThreadId, threadDelay, killThread, forkIO, myThreadId )
import Control.Concurrent.MVar ( MVar, readMVar, swapMVar, newMVar )
import Control.Monad as CM
import Utilities ( elapsed_hundreths )

data HttpPoller = HttpPoller { name :: String
                             , base_request :: Request
                             , handle_response :: String -> IO ()
                             , delay_holder :: MVar Int
                             , p_tid :: ThreadId
                             , manager :: Manager }


start_poller :: String -> Request -> (String -> IO ()) -> Int -> IO HttpPoller
start_poller n b hdlr p = do { tid <- myThreadId
                             ; del <- newMVar p
                             ; mgr <- newManager tlsManagerSettings
                             ; let pre_h = HttpPoller n b hdlr del tid mgr
                             ; new_tid <- forkIO $ poller_loop pre_h Nothing Nothing
                             ; L.infoM n $ "Forked new HttpPoller for " ++ (show $ host b)
                                           ++ (show $ path b)
                                           ++ " with thread ID " ++ (show new_tid) ++ "."
                             ; return $ pre_h { p_tid = new_tid } }

stop_poller :: HttpPoller -> IO ()
stop_poller = killThread . p_tid

change_polling_frequency :: HttpPoller -> Int -> IO ()
change_polling_frequency p d = do { o <- swapMVar (delay_holder p) $ d
                                  ; L.infoM (name p) $ "Changing delay to " ++ (show d)
                                                ++ " microseconds from " ++ (show o) ++ " microseconds." }

poller_loop :: HttpPoller -> Maybe String -> Maybe String -> IO ()
poller_loop p me mlm =
    do { (e,lm,cont) <-
             do { ct_start <- DTC.getCurrentTime
                ; let req = (add_header "If-None-Match" me) . (add_header "If-Modified-Since" mlm)
                            $ base_request p
                ; L.debugM (name p) $ "Performing HTTP request to " ++ (show $ host req) ++ (show $ path req)
                ; resp <- (httpLbs req (manager p))
                ; ct_stop <- DTC.getCurrentTime
                ; L.infoM (name p) $ logtime (base_request p) ct_stop ct_start
                ; L.debugM (name p) $ "Response status: " ++ (show $ responseStatus resp)
                ; handle_ p me mlm resp
                }
         `E.catch` \ex -> do { inner_cont <- handle_http_exception (name p) ex
                            ; return (me,mlm,inner_cont) }
         `E.catch` \ex -> do { inner_cont <- handle_exception (name p) ex
                            ; return (me,mlm,inner_cont) }
       ; CM.when cont $
             do { del <- readMVar $ delay_holder p
                ; L.infoM (name p) $ "Sleeping for "
                              ++ (show del) ++ " microseconds."
                ; loop <-
                    do { threadDelay del
                       ; return True }
                  `E.catch` (handle_exception (name p))
                ; CM.when loop $ poller_loop p e lm
                }
       }

handle_ :: HttpPoller -> Maybe String -> Maybe String -> Response LBS.ByteString -> IO (Maybe String, Maybe String, Bool)
handle_ p _ _ r | statusCode (responseStatus r) == 200 =
    do { L.infoM (name p) $ "Server returned a 200; processing response body."
       ; handle_response p $ BS.unpack . LBS.toStrict $ responseBody r
       ; let e = fmap BS.unpack $ lookup (CI.mk $ BS.pack "ETag") (responseHeaders r)
       ; case e of
           Nothing ->
               L.infoM (name p) "No ETag header present."
           Just hdr ->
               L.infoM (name p) $ "Found ETag " ++ hdr
       ; let lm = fmap BS.unpack $ lookup (CI.mk $ BS.pack "Last-Modified") (responseHeaders r)
       ; case lm of
           Nothing ->
               L.infoM (name p) "No last modified timestamp header present."
           Just hdr ->
               L.infoM (name p) $ "Found last modified timestamp of " ++ hdr
       ; return (e, lm, True)
       }
handle_ p me mlm r | statusCode (responseStatus r) == 304 =
    do { L.infoM (name p) $ "Server returned a 304; nothing to do."
       ; return (me, mlm, True)
       }
handle_ p me mlm r | statusCode (responseStatus r) `elem` [301, 302] =
    do { let location = fmap BS.unpack $ lookup (CI.mk $ BS.pack "Location") (responseHeaders r)
       ; L.infoM (name p) $ "Server returned a " ++ (show $ statusCode $ responseStatus r)
                       ++ " with new location " ++ (fromMaybe "" location)
       ; return (me, mlm, True)
       }
handle_ p me mlm r =
    do { let status = responseStatus r
       ; L.errorM (name p) $ "Server returned an unexpected response " ++ (show $ statusCode status)
                   ++ " " ++ (BS.unpack $ statusMessage status)
       ; return (me, mlm, True)
       }

add_header :: String -> Maybe String -> Request -> Request
add_header _ Nothing req = req
add_header h (Just s) req = req { requestHeaders = (CI.mk $ BS.pack h, BS.pack s) : requestHeaders req }

logtime :: Request -> DTC.UTCTime -> DTC.UTCTime -> String
logtime req ct_stop ct_start = "Took " ++ (elapsed_hundreths ct_stop ct_start) ++ " seconds to perform "
                               ++ (show $ method req) ++ " "
                               ++ (show $ host req) ++ (show $ path req)

handle_http_exception :: String -> HttpException -> IO Bool
handle_http_exception hnd ex =
    do { L.errorM hnd $ "HTTP exception during operation: " ++ show ex
       ; return True }

handle_exception :: String -> E.SomeException -> IO Bool
handle_exception hnd e =
    do { L.errorM hnd $ "Unexpected exception encountered; continuing.  Exception was: " ++ (show e)
       ; return True }

fmt :: String -> String
fmt s = (take l ps) ++ "." ++ ((drop l) ps)
    where
      ps = pad 3 '0' s
      l = (length ps) - 2

pad :: Int -> Char -> String -> String
pad i c s | length s >= i = s
          | otherwise = pad (i-1) c (c:s)
