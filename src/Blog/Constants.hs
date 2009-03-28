-- Constants.hs
module Blog.Constants where

anonymous_author :: String
anonymous_author = "anonymous coward"

blog_title :: String
blog_title = "mult.ifario.us"

blog_title_for_urn :: String
blog_title_for_urn = blog_title

blog_tagline :: String
blog_tagline = "Software, business, and random related items."

blog_root :: String
blog_root = "http://mult.ifario.us"

author_name :: String
author_name = "Paul R. Brown"

author_email :: Maybe String
author_email = Just "paulrbrown@gmail.com"

author_uri :: Maybe String
author_uri = Just base_url

generator_name :: String
generator_name = "perpubplat"

generator_uri :: String
generator_uri = "http://datapr0n.com/perpubplat"

generator_version :: String
generator_version = "0.10.0"

generator_desc :: String
generator_desc = "Paul's Personal Publishing Platform"

content_root :: String
content_root  = "/Users/prb/web/perpubplat"

content_storage_dir :: String
content_storage_dir = content_root ++ "/content"

draft_dir :: String
draft_dir = content_root ++ "/drafts"

comment_dir :: String
comment_dir = content_root ++ "/comments"

data_dir :: String
data_dir = content_root ++ "/data"

sqlite_hits_db_filename :: String
sqlite_hits_db_filename = data_dir ++ "/hits.db"

base_url :: String
base_url = "http://mult.ifario.us"

stylesheet_url :: String
stylesheet_url = "/static/styles.css"

default_page_size :: Int
default_page_size = 7

feed_length :: Int
feed_length = 25

comment_view_size :: Int
comment_view_size = 30

license_xhtml :: String
license_xhtml = "<a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/us/\">"
                ++ "<img alt=\"Creative Commons License\" class=\"cc-badge\" "
                ++ "src=\"http://i.creativecommons.org/l/by-sa/3.0/us/80x15.png\" /></a>"
                ++ " Except where otherwise noted, this work is licensed under a "
                ++ "<a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/us/\">Creative Commons Attribution-Share Alike 3.0 United States License</a>."

tags_to_show :: Int
tags_to_show = 40

first_datetime :: String
first_datetime = "1970-01-01T00:00:00Z"

twitter_user :: String
twitter_user = ""

twitter_pass :: String
twitter_pass = "" -- fill yours in

twitter_count :: Int
twitter_count = 10

flickr_user :: String
flickr_user = "" -- fill yours in

flickr_api_key :: String
flickr_api_key = "" -- fill yours in

delicious_user :: String
delicious_user = "" -- fill yours in

logfile :: String
logfile = content_root ++ "/logs/perpubplat.log"

google_user :: String
google_user = "" -- fill yours in