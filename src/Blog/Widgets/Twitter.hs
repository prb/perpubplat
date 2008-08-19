module Blog.Widgets.Twitter (boot_twitter, get_tweets, TwitterController ( poller, twitter_tid )) where

import Text.ParserCombinators.Parsec

import qualified Debug.Trace as D

import Network.HTTP
import Network.HTTP.Headers
import Network.URI ( parseURI )
import Data.Maybe ( fromJust )
import Data.List ( elemIndex, intersperse, isPrefixOf )
import Text.XHtml.Strict
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import qualified Codec.Binary.Base64.String as B64

import qualified Blog.Constants as C
import Blog.FrontEnd.ContentAtoms (divid, _a) 
import Blog.Widgets.JsonUtilities

import Blog.BackEnd.HttpPoller

data TRequest = GetTweets ( MVar String )
              | UpdateTweets String

data TwitterController = TwitterController { request_channel :: Chan TRequest
                                           , poller :: HttpPoller
                                           , twitter_tid :: ThreadId }

boot_twitter :: String -> String -> Int -> IO TwitterController
boot_twitter user password count = do { let req = build_tweet_request user password count
                                      ; rc <- newChan
                                      ; p <- start_poller "Tweets" req (handle_body rc) (120 * 10^6)
                                      ; tid <- forkIO $ loop rc ""
                                      ; return $ TwitterController rc p tid }

loop :: Chan TRequest -> String -> IO ()
loop rc xh = do { req <- readChan rc
                ; case req of
                    GetTweets hb ->
                        putMVar hb xh >> loop rc xh
                    UpdateTweets xh' ->
                        loop rc xh' }

build_tweet_request :: String -> String -> Int -> Request
build_tweet_request user password count = Request uri GET heads ""
    where 
      uri = fromJust $ parseURI $ "http://twitter.com/statuses/user_timeline/"
            ++ user ++ ".json?count=" ++ (show count)
      heads = [ Header HdrAuthorization $ (++) "Basic " $ B64.encode $ user ++ ":" ++ password ]

handle_body :: Chan TRequest -> String -> IO ()
handle_body tc body = do { case parse_json body of
                             Right v ->
                                 do { let texts = map uns $ una $ v </> "text"
                                    ; let times = map (convert_twitter_tstmp . uns) $ una $ v </> "created_at"
                                    ; let ids = map (show . unn) $ una $ v </> "id"
                                    ; update_tweets tc $ tweets_to_xhtml $ zip3 times texts ids }
                             Left err ->
                                 D.putTraceMsg . show $ err }

send :: TwitterController -> TRequest -> IO ()
send tc = writeChan (request_channel tc)

update_tweets :: Chan TRequest -> String -> IO ()
update_tweets tc tweets = writeChan tc $ UpdateTweets tweets

get_tweets :: TwitterController -> IO String
get_tweets tc = do { hb <- newEmptyMVar
                   ; send tc $ GetTweets hb
                   ; takeMVar hb }

convert_twitter_tstmp :: String -> String
convert_twitter_tstmp ts = concat [ y, "-", mo', "-", d, "T", tm, "Z" ]
    where
      mo = take 3 $ drop 4 ts
      mo' = pad $ 1 + ( fromJust $ elemIndex mo [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
                                                , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ] )
      pad = \n -> if n <10 then ('0':show n) else show n
      y = take 4 $ drop 26 ts
      d = take 2 $ drop 8 ts
      tm = take 8 $ drop 11 ts

tweets_to_xhtml :: [(String,String,String)] -> String
tweets_to_xhtml = showHtmlFragment . (divid "tweets") . (build_tweet_list "1970-01-01")

build_tweet_list :: String -> [(String,String,String)] -> Html
build_tweet_list _ [] = noHtml
build_tweet_list last_date ((d,t,tweet_id):dts)
    = concatHtml [ if last_date `isPrefixOf` d then
                       noHtml
                   else
                       (p ! [theclass "tweet_group" ]) . stringToHtml $ date
                 , p ! [theclass "tweet"] $ concatHtml [ _a tweet_link time
                                                        , stringToHtml " "
                                                        , text ]
                 , build_tweet_list date dts ]
      where
        wrap st = (thespan ! [ theclass st ])
        time_hunk = (take 9) . (drop 11)
        time = wrap "tweet_stamp" $ stringToHtml . time_hunk $ d
        date = take 10 d
        text = wrap "tweet_text" $ primHtml . pre_process $ t
        tweet_link = "http://twitter.com/" ++ C.twitter_user ++ "/statuses/"
                     ++ tweet_id
        
pre_process :: String -> String
pre_process s = case parse  pre_process_parser "" s of
                 Left err -> error . show $ err
                 Right v -> v


pre_process_parser :: Parser String
pre_process_parser = do { ts <- tok `sepBy` (many1 space)
                        ; return $ concat . (intersperse " ") $ ts }

tok :: Parser String
tok = try http_link <|> try at_someone <|> word

word :: Parser String
word = many1 $ noneOf " "

at_someone :: Parser String
at_someone = do { char '@'
                ; s <- many1 $ noneOf " "
                ; return $ "<a href=\"http://twitter.com/" ++ (urlEncode s) ++ "\">@" ++ s ++ "</a>" }

http_link :: Parser String
http_link = do { string "http://"
               ; s <- many1 $ noneOf " "
               ; return $ "<a href=\"http://" ++ s ++ "\">" ++ s ++ "</a>" }

