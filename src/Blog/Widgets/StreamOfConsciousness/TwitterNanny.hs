module Blog.Widgets.StreamOfConsciousness.TwitterNanny ( start_twitter_nanny ) where

import qualified Control.Monad as CM
import Blog.Widgets.StreamOfConsciousness.Controller ( Worker ( .. )
                                                     , SoCController )

import qualified System.Log.Logger as L

import Network.HTTP.Client
import qualified Data.ByteString.Char8 as BS
import Data.Maybe ( fromJust )


import qualified Codec.Binary.Base64.String as B64

import Blog.Widgets.JsonUtilities

import Blog.BackEnd.HttpPoller

nanny_period :: Int
nanny_period = 60 * 10^6 -- supposedly cheap.

log_handle :: String
log_handle = "TwitterNanny"

start_twitter_nanny :: SoCController -> [(Worker,Int)] -> String -> String -> IO Worker
start_twitter_nanny socc kids user password = do { let req = build_request user password
                                                 ; p <- start_poller log_handle req (handle_throttle kids) nanny_period
                                                 ; return $ Worker socc p }

build_request :: String -> String -> Request
build_request user password =
    let url = "http://twitter.com/account/rate_limit_status.json"
        authValue = "Basic " ++ B64.encode (user ++ ":" ++ password)
    in (fromJust $ parseRequest url)
       { requestHeaders = [(BS.pack "Authorization", BS.pack authValue)] }

handle_throttle :: [(Worker,Int)] -> String -> IO ()
handle_throttle ws body = case parse_utf8_json body of
                            Right v@(JSObject _) ->
                                compute_new_delays ws v
                            Right _ ->
                                L.errorM log_handle $ "Unexpected non-object JSON response that starts with: "
                                     ++ (take 100 body) ++ " [...]"
                            Left err_message ->
                                L.errorM log_handle err_message
                               
compute_new_delays :: [(Worker,Int)] -> JSValue -> IO ()
compute_new_delays _ v = do { let throttle = unn $ v </> "remaining_hits"
                             ; L.infoM log_handle $ "Twitter throttle is " ++ (show throttle) ++ " requests per hour."
                             }
