module Blog.Widgets.StreamOfConsciousness.Twitter ( start_twitter_tweets, start_twitter_replies ) where

import qualified Blog.Widgets.StreamOfConsciousness.Thought as T
import Blog.Widgets.StreamOfConsciousness.Controller

import Data.List (isPrefixOf)
import Text.ParserCombinators.Parsec

import qualified System.Log.Logger as L

import Network.HTTP
import Network.HTTP.Headers
import Network.URI ( parseURI )
import Data.Maybe ( fromJust )
import Data.List ( elemIndex, intersperse )

import qualified Text.XHtml.Strict as X
import Text.JSON

import qualified Codec.Binary.Base64.String as B64

import Blog.Widgets.JsonUtilities

import Blog.BackEnd.HttpPoller

data Kind = Tweet | Reply

method :: Kind -> String
method Tweet = "user_timeline"
method Reply = "replies"

handler :: Kind -> SoCController -> String -> String -> IO ()
handler Tweet = handle_tweets
handler Reply = handle_replies

log_handle :: Kind -> String
log_handle Tweet = "TwitterTweets"
log_handle Reply = "TwitterReplies"

twitter_period :: Int
twitter_period  = 15 * 60 * 10^6 -- fifteen minutes in milliseconds

start_twitter_tweets :: SoCController -> String -> String -> IO Worker
start_twitter_tweets = start_twitter Tweet

start_twitter_replies :: SoCController -> String -> String -> IO Worker
start_twitter_replies = start_twitter Reply

start_twitter :: Kind -> SoCController -> String -> String -> IO Worker
start_twitter kind socc user password
    = do { let req = build_request kind user password
         ; p <- start_poller (log_handle kind) req ((handler kind) socc user)
               twitter_period
         ; return $ Worker socc p }

build_request :: Kind -> String -> String -> Request
build_request kind user password = Request uri GET heads ""
    where
      uri = fromJust $ parseURI $ "http://twitter.com/statuses/"
            ++ (method kind) ++ ".json"
      heads = [ Header HdrAuthorization $ (++) "Basic " $ B64.encode $ user ++ ":" ++ password ]

handle_tweets :: SoCController -> String -> String -> IO ()
handle_tweets socc user body
    = case parse_utf8_json body of
        Right v@(JSArray _) ->
            commit socc $ tweets_to_thoughts user v
        Right _ ->
            L.errorM (log_handle Tweet) $ "Unexpected non-array JSON response that starts with: "
                 ++ (take 100 body) ++ " [...]"
        Left err_message ->
            L.errorM (log_handle Tweet) err_message

tweets_to_thoughts :: String -> JSValue -> [T.Thought]
tweets_to_thoughts user v = map (\ (d,u,t) -> T.Thought T.TwitterTweet d u t) $ zip3 times urls texts
    where
      texts = map (tweet_body_to_xhtml . uns) $ una $ v </> "text"
      times = map (convert_twitter_tstmp . uns) $ una $ v </> "created_at"
      urls = map ((tweet_id_to_link user) . show . unn) $ una $ v </> "id"

handle_replies :: SoCController -> String -> String -> IO ()
handle_replies socc user body
    = case parse_utf8_json body of
        Right v@(JSArray _) ->
            commit socc $ replies_to_thoughts user v
        Right _ ->
            L.errorM (log_handle Reply) $ "Unexpected non-array JSON response that starts with: "
                 ++ (take 100 body) ++ " [...]"
        Left err_message ->
            L.errorM (log_handle Reply) err_message

replies_to_thoughts :: String -> JSValue -> [T.Thought]
replies_to_thoughts user v = map (\ (d,u,t) -> T.Thought T.TwitterReply d u t) $ zip3 times urls texts
    where
      tweet_ids = map (show . unn) $ una $ v </> "id"
      users = uns_ $ v </> "user" </> "screen_name"
      urls = map (uncurry tweet_id_to_link) $ zip users tweet_ids
      pre_texts = uns_ $ v </> "text"
      texts = map (tweet_body_to_xhtml . (uncurry $ process_reply user)) $ zip users pre_texts
      times = map convert_twitter_tstmp $ uns_ $ v </> "created_at"

tweet_id_to_link :: String -> String -> String
tweet_id_to_link user t_id = "http://twitter.com/" ++ user ++ "/statuses/" ++ t_id

tweet_body_to_xhtml :: String -> String
tweet_body_to_xhtml = X.showHtmlFragment . (X.thespan X.! [ X.theclass "tweet_text" ])
                      . X.primHtml . pre_process

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

pre_process :: String -> String
pre_process s = case parse pre_process_parser "" s of
                 Left err -> error . show $ err
                 Right v -> v

pre_process_parser :: Parser String
pre_process_parser = do { ts <- tok `sepBy` (many1 space)
                        ; return $ concat . (intersperse " ") $ ts }

tok :: Parser String
tok = try http_link <|> try at_someone <|> try paren_http_link <|> try sqb_at_someone <|> word

word :: Parser String
word = many1 $ noneOf " "

sqb_at_someone :: Parser String
sqb_at_someone = do { string "[@"
                    ; s <- many1 $ noneOf " ]"
                    ; char ']'
                    ; return $ "[from <a href=\"http://twitter.com/" ++ (urlEncode s) ++ "\">@" ++ s ++ "</a>] " }

at_someone :: Parser String
at_someone = do { char '@'
                ; s <- many1 $ noneOf " "
                ; return $ "<a href=\"http://twitter.com/" ++ (urlEncode s) ++ "\">@" ++ s ++ "</a>" }

http_link :: Parser String
http_link = do { string "http://"
               ; s <- many1 $ noneOf " "
               ; return $ "<a href=\"http://" ++ s ++ "\">" ++ s ++ "</a>" }

paren_http_link :: Parser String
paren_http_link = do { string "(http://"
                     ; s <- many1 $ noneOf " )"
                     ; char ')'
                     ; return $ "(<a href=\"http://" ++ s ++ "\">" ++ s ++ "</a>)" }

process_reply :: String -> String -> String -> String 
process_reply user from_user body = "[@" ++ from_user ++ "] " ++ userless_body
    where
      ul = length user
      userless_body = if ('@':user) `isPrefixOf` body then
                          drop (ul + 1) body
                      else 
                          body
                          