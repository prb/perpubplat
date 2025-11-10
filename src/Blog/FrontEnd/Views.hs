module Blog.FrontEnd.Views
    ( View ( All, ByYear, ByMonth, ByDay,
             ByTag, ByTags,
             ByYMDPermatitle, ByPermatitle,
             year, month, day, page_n, permalink, tag, tags
           ),
      Viewable ( url, kind, lens, title, discoverable_feeds,
                 page_number, page_size, no_posts_message, next_page,
                 first_page ),
      ViewKind ( Single, Multiple ),
      page, page_count
    ) where

import qualified Blog.Constants as C
import qualified Blog.Model.Entry as B
import qualified Blog.FrontEnd.Feeds as F
import Data.Maybe (maybeToList)
import Utilities
import Data.List (intersperse)

data View = All { page_n :: Maybe Int }
          | ByYear { year :: Int, page_n :: Maybe Int }
          | ByMonth { year :: Int, month :: Int, page_n :: Maybe Int }
          | ByDay { year :: Int, month :: Int, day :: Int, page_n :: Maybe Int }
          | ByTags { tags :: [String], page_n :: Maybe Int }
          | ByTag { tag :: String, page_n :: Maybe Int }
          | ByYMDPermatitle { year :: Int, month :: Int, day :: Int,
                              permalink :: String }
          | ByPermatitle { permalink :: String }
            deriving ( Show, Eq )

data ViewKind = Single | Multiple
                deriving ( Show, Eq )

class Viewable v where
    url :: v -> String
    kind :: v -> ViewKind
    lens :: v -> B.Model -> [B.Item]
    title :: v -> String
    discoverable_feeds :: v -> [F.DiscoverableFeed]
    page_number :: v -> Maybe Int
    page_size :: v -> Int
    no_posts_message :: v -> String
    next_page :: v -> v
    first_page :: v -> v

url_ :: View -> String
url_ (All _) = "/a"
url_ (ByYear y _) = "/a/" ++ (show y)
url_ (ByTag t _) = "/t/" ++ t
url_ (ByTags t _) = "/t/" ++ (concat $ intersperse "," t)
url_ (ByMonth y m _) = "/a/" ++ (show y) ++ "/" ++ (pad_ m)
url_ (ByDay y m d _) = "/a/" ++ (show y) ++ "/" ++ (pad_ m) ++ "/" ++ (pad_ d)
url_ (ByPermatitle t) = "/p/" ++ t
url_ (ByYMDPermatitle _ _ _ t) = "/p/" ++ t

                            
instance Viewable View where
    page_number (ByYMDPermatitle _ _ _ _) = Nothing
    page_number (ByPermatitle _) = Nothing
    page_number v = page_n v

    url v = (url_ v) ++ ((page_suffix . page_number) v)

    page_size (All _) = C.default_page_size
    page_size (ByYear _ _) = C.default_page_size
    page_size (ByMonth _ _ _) = C.default_page_size
    page_size (ByDay _ _ _ _) = C.default_page_size
    page_size (ByTag _ _) = C.default_page_size
    page_size (ByTags _ _) = C.default_page_size
    page_size (ByYMDPermatitle _ _ _ _) = 1
    page_size (ByPermatitle _) = 1
    
    no_posts_message (ByYear y _) = "No posts from year " ++ (show y) ++ " were found."
    no_posts_message (ByMonth y m _) = "No posts from " ++ (show y) ++ (pad_ m) ++ " were found."
    no_posts_message (ByDay y m d _) = "No posts were made on " ++ (show y) ++ (pad_ m) ++ (pad_ d) ++ "."
    no_posts_message (ByTags t _) = "No posts are in the intersection of the tags {" ++
                                    (concat $ intersperse "," t) ++ "}."
    no_posts_message (ByTag t _) = "No posts are tagged " ++ t ++ "."
    no_posts_message (ByYMDPermatitle y m d t) = "No posts with permatitle " ++ t ++ " were made on"
                                                 ++ (show y) ++ (pad_ m) ++ (pad_ d)
                                                 ++ "; try the same URL without the "
                                                 ++ "/" ++ (show y) ++ "/" ++ (pad_ m) ++ "/"
                                                 ++ (pad_ d) ++ " fragment."
    no_posts_message (ByPermatitle t) = "No post with permatitle " ++ t ++ " was found."
    no_posts_message (All _) = "No posts were found at all...  Something's fishy."
    
    discoverable_feeds (All _) = [F.articles_feed, F.all_comments_feed]
    discoverable_feeds (ByYear _ _) = [F.articles_feed, F.all_comments_feed]
    discoverable_feeds (ByMonth _ _ _) = [F.articles_feed, F.all_comments_feed]
    discoverable_feeds (ByDay _ _ _ _) = [F.articles_feed, F.all_comments_feed]
    discoverable_feeds (ByYMDPermatitle _ _ _ t) = [F.articles_feed, F.all_comments_feed, F.comments_feed t]
    discoverable_feeds (ByPermatitle t) = [F.articles_feed, F.all_comments_feed, F.comments_feed t]
    discoverable_feeds (ByTag t _) = [F.articles_feed, F.all_comments_feed, F.tags_feed [t]]
    discoverable_feeds (ByTags t _) = [F.articles_feed, F.all_comments_feed, F.tags_feed t]

    kind (All _) = Multiple
    kind (ByYear _ _) = Multiple
    kind (ByMonth _ _ _) = Multiple
    kind (ByDay _ _ _ _) = Multiple
    kind (ByTag _ _) = Multiple
    kind (ByTags _ _) = Multiple
    kind (ByYMDPermatitle _ _ _ _) = Single
    kind (ByPermatitle _) = Single

    lens (All _) = (filter $ ((==) B.Post) . B.kind) . B.all_posts
    lens (ByYear y _) = (B.year_filter y) . B.all_posts
    lens (ByMonth y m _) = (B.month_filter y m) . B.all_posts
    lens (ByDay y m d _) = (B.day_filter y m d) . B.all_posts
    lens (ByTag t _) = (B.tag_filter t) . B.all_posts
    lens (ByTags t _) = (B.tags_filter t) . B.all_posts
    lens (ByYMDPermatitle _ _ _ t) = maybeToList . ((flip B.maybe_post_by_permatitle) t)
    lens (ByPermatitle t) = maybeToList . ((flip B.maybe_post_by_permatitle) t)

    title (All _) = "All Posts"
    title (ByYear y _) = show y
    title (ByMonth y m _) = (show y) ++ "/" ++ (show m)
    title (ByDay y m d _) = (show y) ++ "/" ++ (show m) ++ "/" ++ (show d)
    title (ByTag t _) = "Posts tagged \"" ++ t ++ "\""
    title (ByTags t _) = "Posts tagged " ++ (show t)
    title (ByYMDPermatitle _ _ _ t) = t
    title (ByPermatitle t) = t

    first_page (All _) = All $ Just 1
    first_page (ByYear y _) = ByYear y $ Just 1
    first_page (ByMonth y m _) = ByMonth y m $ Just 1
    first_page (ByDay y m d _) = ByDay y m d $ Just 1
    first_page (ByTag t _) = ByTag t $ Just 1
    first_page (ByTags t _) = ByTags t $ Just 1

    next_page (All Nothing) = All $ Just 2
    next_page (All (Just a)) = All $ Just (a+1)
    next_page (ByYear y Nothing) = ByYear y $ Just 2
    next_page (ByYear y (Just a)) = ByYear y $ Just (a+1)
    next_page (ByMonth y m Nothing) = ByMonth y m $ Just 2
    next_page (ByMonth y m (Just a)) = ByMonth y m $ Just (a+1)
    next_page (ByDay y m d Nothing) = ByDay y m d $ Just 2
    next_page (ByDay y m  d (Just a)) = ByDay y m d $ Just (a+1)
    next_page (ByTag t Nothing) = ByTag t $ Just 2
    next_page (ByTag t (Just a)) = ByTag t $ Just (a+1)
    next_page (ByTags ts Nothing) = ByTags ts $ Just 2
    next_page (ByTags ts (Just a)) = ByTags ts $ Just (a+1)



page_suffix :: Maybe Int -> String
page_suffix Nothing = ""
page_suffix (Just n) = "/page/" ++ (show n)

page :: (Viewable v) => v -> [a] -> [a]
page v = paginate (page_size v) (one_if_nothing $ page_number v)

one_if_nothing :: Maybe Int -> Int
one_if_nothing Nothing = 1
one_if_nothing (Just s) = s

page_count :: (Viewable v) => v -> B.Model -> Int
page_count w m = last_page (page_size w) ((lens w) m)

pad_ :: Int -> String
pad_ n | n < 10 = '0':(show n)
       | otherwise = show n