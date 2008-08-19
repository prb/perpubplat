module Blog.BackEnd.HttpPoller where

import Data.Char
import Data.Maybe

import qualified System.Log.Logger as L

import Network.HTTP
import Network.HTTP.Headers

import qualified Control.Exception as E
import System.Time
import Control.Concurrent ( ThreadId, threadDelay, killThread, forkIO, myThreadId )
import Control.Concurrent.MVar ( MVar, readMVar, swapMVar, newMVar )
import Control.Monad as CM
import Utilities ( elapsed_hundreths )

data HttpPoller = HttpPoller { name :: String
                             , base_request :: Request
                             , handle_response :: String -> IO ()
                             , delay_holder :: MVar Int
                             , p_tid :: ThreadId }


start_poller :: String -> Request -> (String -> IO ()) -> Int -> IO HttpPoller
start_poller n b hdlr p = do { tid <- myThreadId
                             ; del <- newMVar p
                             ; let pre_h = HttpPoller n b hdlr del tid
                             ; new_tid <- forkIO $ poller_loop pre_h Nothing Nothing
                             ; L.infoM n $ "Forked new HttpPoller for " ++ ( show . rqURI $ b)
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
             do { ct_start <- getClockTime
                ; let req = (add_maybe HdrIfNoneMatch me) . (add_maybe HdrIfModifiedSince mlm)
                            $ base_request p
                ; L.debugM (name p) $ "Performing HTTP request " ++ (show req)
                ; resp <- simpleHTTP req
                ; ct_stop <- getClockTime
                ; L.infoM (name p) $ logtime (base_request p) ct_stop ct_start
                ; case resp of
                    Left e ->
                        do { L.errorM (name p) $ "Error connecting to " ++ ( show . rqURI $ req )
                                          ++ ": " ++ (show e)
                           ; return (me,mlm,True) }
                    Right r -> 
                        do { L.debugM (name p) $ show r
                           ; handle_ p me mlm r }
                }
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

handle_ :: HttpPoller -> Maybe String -> Maybe String -> Response -> IO (Maybe String, Maybe String, Bool)
handle_ p _ _ r@(Response (2,0,0) _ _ body) =
    do { L.infoM (name p) $ "Server returned a 200; processing response body."
       ; handle_response p $ body 
       ; let e = findHeader HdrETag r
       ; case e of 
           Nothing ->
               L.infoM (name p) "No ETag header present."
           Just hdr ->
               L.infoM (name p) $ "Found ETag " ++ hdr
       ; let lm = findHeader HdrLastModified r
       ; case lm of 
           Nothing ->
               L.infoM (name p) "No last modified timestamp header present."
           Just hdr ->
               L.infoM (name p) $ "Found last modified timestamp of " ++ hdr
       ; return (e, lm, True)
       }
handle_ p me mlm (Response (3,0,4) _ _ _) =
    do { L.infoM (name p) $ "Server returned a 304; nothing to do."
       ; return (me, mlm, True)
       }
handle_ p me mlm r@(Response rc@(3,0,k) _ _ _) | k == 1 || k == 2 =
                                              do { L.infoM (name p) $ "Server returned a " ++ (show_rc rc)
                                                               ++ " with new location " ++ (fromMaybe "" $ findHeader HdrLocation r)
                                                 ; return (me, mlm, True)
                                                 } 
handle_ p me mlm (Response rc reasn _ _) =
    do { L.errorM (name p) $ "Server returned an unexpected response " ++ (show_rc rc) ++ " " ++ reasn
       ; return (me, mlm, True)
       }

add_maybe :: HeaderName -> Maybe String -> Request -> Request
add_maybe _ Nothing req = req
add_maybe h (Just s) req = req { rqHeaders = (Header h s):(rqHeaders req) }

logtime :: Request -> ClockTime -> ClockTime -> String
logtime req ct_stop ct_start = "Took " ++ (elapsed_hundreths ct_stop ct_start) ++ " seconds to perform "
                               ++ (show . rqMethod $ req ) ++ " "
                               ++ (show . rqURI $ req )
                     
handle_exception :: String -> E.Exception -> IO Bool
handle_exception hnd (E.ErrorCall msg) =
    do { L.errorM  hnd $ "Exception during HTTP operation: " ++ msg
       ; return True }
handle_exception hnd (E.AsyncException E.ThreadKilled) =
    do { L.errorM hnd $ "Kill received; exiting gracefully."
       ; return False }
handle_exception hnd (E.IOException ex) =
    do { L.errorM hnd $ "IOException encountered: " ++ show ex
       ; return True }
handle_exception hnd e =
    do { L.errorM hnd $ "Unexpected exception encountered; stopping poller.  Exception was: " ++ (show e)
       ; return False } 

fmt :: String -> String
fmt s = (take l ps) ++ "." ++ ((drop l) ps)
    where
      ps = pad 3 '0' s
      l = (length ps) - 2

pad :: Int -> Char -> String -> String
pad i c s | length s >= i = s
          | otherwise = pad (i-1) c (c:s)

show_rc :: ResponseCode -> String
show_rc (h,t,o) = [ digit h, digit t, digit o ]
    where
      digit = chr . (ord '0' + )
