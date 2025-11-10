module Blog.FrontEnd.Feeds
    ( Feed ( AllPosts, ByTag, ByTags, PostComments, AllComments, tag, tags, permalink),
      DiscoverableFeed ( feed_title, feed_url ),
      Feedable ( items, categories, build_id, title, self_url, home_url, discoverable_feed ),
      articles_feed, all_comments_feed, comments_feed, tags_feed
    ) where

import qualified Blog.FrontEnd.Urls as U
import qualified Blog.Constants as C
import qualified Blog.Model.Entry as B

import Data.List (intersperse)

data Feed = AllPosts
          | ByTag { tag :: String }
          | ByTags { tags :: [String] }
          | PostComments { permalink :: String }
          | AllComments 
            deriving ( Show, Eq )

type Iri = String

data DiscoverableFeed = DiscoverableFeed { feed_title :: String, feed_url :: String }
                        deriving ( Show )

class Feedable f where
    -- | construct the posts for the feed from a list of all visible posts in time-sorted order
    items :: f -> B.Model -> [B.Item]
    -- | construct a set of categories, potentially empty
    categories :: f -> [String]
    -- | construct the @rel="self"@ URL for the feed
    self_url :: f -> String
    -- | construct the home URL for the feed
    home_url :: f -> String
    -- | construct the @atom:id@ for the feed
    build_id :: f -> Iri
    -- | construct the title for the feed
    title :: f -> String
    -- | construct the autodiscovery feed for this feed
    discoverable_feed :: f -> DiscoverableFeed
    discoverable_feed f = DiscoverableFeed { feed_title = title f, feed_url = self_url f}

instance Feedable Feed where
    items AllPosts m = (take C.feed_length) . B.all_posts $ m
    items (ByTag t) m = (take C.feed_length) . (B.tag_filter t) . B.all_posts $ m 
    items (ByTags t) m = (take C.feed_length) . (B.tags_filter t) . B.all_posts $ m
    items AllComments m = ((take C.feed_length) . (B.concat_comments m) . B.all_posts) m
    items (PostComments t) m = ((take C.feed_length) . (B.concat_comments m) . (B.plink_filter t) . B.all_posts) m

    categories (AllPosts) = []
    categories (ByTag t) = [t]
    categories (ByTags t) = t
    categories (AllComments) = []
    categories (PostComments _) = []

    self_url AllPosts = feed_ "a"
    self_url (ByTag t) = feed_ ("t/" ++ t)
    self_url (ByTags t) = feed_ ("t/" ++ (U.tags_fragment t))
    self_url (PostComments t) = feed_ ("c/p/" ++ t)
    self_url AllComments = feed_ "c"

    home_url AllPosts = U.all_posts
    home_url (ByTag t) = U.posts_by_tag t
    home_url (ByTags t) = U.posts_by_tags t
    home_url (PostComments t) = U.post t
    home_url AllComments = U.all_posts

    title AllPosts = C.blog_title ++ " - All Posts"
    title (ByTags t) = C.blog_title ++ " - Posts Tagged {"
                                              ++ (concat $ intersperse ", " t) ++ "}"
    title (ByTag t) = C.blog_title ++ " - Posts Tagged {" ++ t ++ "}"
    title (PostComments t) = C.blog_title ++ " - Comments on " ++ t
    title AllComments = C.blog_title ++ " - Comments on Recent Posts"


    build_id AllPosts = ppp_urn ++ "posts:atom:all"
    build_id (ByTags t) = ppp_urn ++ "posts:atom:by_tags(" ++ (U.tags_fragment t) ++ ")"
    build_id (ByTag t) = ppp_urn ++ "posts:atom:by_tags(" ++ t ++ ")"
    build_id (PostComments t) = ppp_urn ++ "comments:atom:by_permatitle(" ++ t ++ ")"
    build_id AllComments = ppp_urn ++ "comments:atom:all"

ppp_urn :: String
ppp_urn = "urn:perpubplat:" ++ C.blog_title_for_urn ++ ":"

feed_ :: String -> String
feed_ s = C.base_url ++ "/f/" ++ s ++ "/atom.xml"

articles_feed :: DiscoverableFeed
articles_feed = discoverable_feed AllPosts

all_comments_feed :: DiscoverableFeed
all_comments_feed = discoverable_feed AllComments

comments_feed :: String -> DiscoverableFeed
comments_feed = discoverable_feed . PostComments

tags_feed :: [String] -> DiscoverableFeed
tags_feed = discoverable_feed . ByTags

-- tag_feed :: String -> DiscoverableFeed
-- tag_feed = discoverable_feed . ByTag
