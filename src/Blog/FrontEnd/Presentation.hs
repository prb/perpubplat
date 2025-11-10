-- | Module for presentation of entries and lists of entries as XHTML
-- fragments and/or pages, as appropriate.
{-# LANGUAGE OverloadedStrings #-}
module Blog.FrontEnd.Presentation where

import Lucid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Blog.FrontEnd.ContentAtoms
import qualified Blog.Model.Entry as B
import qualified Blog.FrontEnd.Views as V
import qualified Blog.FrontEnd.Feeds as F
import qualified Blog.FrontEnd.Urls as U
import qualified Blog.BackEnd.Holder as H
import qualified Blog.Widgets.FlickrCollage as FC
import qualified Blog.Widgets.ChromeBackEnd as CB
import qualified Blog.Widgets.Delicious as D
import qualified Blog.BackEnd.HitTracker as HitT
import qualified Blog.Widgets.StreamOfConsciousness as SoC
import qualified Codec.Binary.UTF8.String as UTF8

import Data.List

-- | Put the page together, including navigation at the top or bottom,
-- stylesheet, feeds URLs, etc.
assemble_page :: (V.Viewable v) => v -- ^ the kind of page to display
              -> CB.ChromeBackEnd -- ^ a backend for chrome data
              -> B.Model -- ^ all of the posts in the blog
              -> IO String
assemble_page v cb m =
    case paged_posts of
      [] ->
          return ""
      _ ->
          do { sb <- sidebar v cb m paged_posts
             ; phunk <- renderer v cb m $ paged_posts
             ; return . UTF8.encodeString . TL.unpack . renderText $ do
                 topmatter v
                 body_ $ divid "container" $ do
                     heading
                     divid "posts" $ do
                         phunk
                         bottom_nav v m
                     sb
                     footer }
    where
      posts = (V.lens v) m
      paged_posts = (V.page v) $ posts

topmatter :: (V.Viewable v) => v -> Html ()
topmatter v = head_ $ do
    title_ $ toHtml $ V.title v
    base_url
    build_discoverable_feeds v
    stylesheet
    robots
  where
    robots = if (V.kind v == V.Single) then
                 meta_ [name_ "ROBOTS", content_ "INDEX, FOLLOW"]
             else
                 meta_ [name_ "ROBOTS", content_ "NOINDEX, FOLLOW"]

nav_blurb :: (V.Viewable v) => v -> B.Model -> Html ()
nav_blurb w m = do
    strong_ $ toHtml $ V.title w
    toHtml (" contains " :: String)
    strong_ $ toHtml $ show $ length ((V.lens w) m)
    toHtml (" items in " :: String)
    strong_ $ toHtml $ show $ V.page_count w m
    toHtml (" pages of " :: String)
    strong_ $ toHtml $ show $ V.page_size w
    toHtml (" items each: " :: String)

bottom_nav :: (V.Viewable v) => v -> B.Model -> Html ()
bottom_nav w m | (V.kind w == V.Single) || ((V.page_count w m) == 1) = mempty
               | otherwise = divid "bottomnav" $ p_ $ do
                   nav_blurb w m
                   br_ []
                   pages w m

sidebar_nav :: (V.Viewable v) => v -> B.Model -> Html ()
sidebar_nav w m | (V.kind w == V.Single) || ((V.page_count w m) == 1) = mempty
                | otherwise = do
                    h3_ $ toHtml ("Navigation" :: String)
                    p_ [class_ "navigation"] $ do
                        nav_blurb w m
                        br_ []
                        pages w m

pages :: (V.Viewable v) => v -> B.Model -> Html ()
pages w m = mconcat
            $ (intersperse $ toHtml (" " :: String))
            $ page_nav (V.first_page w) n current_page 1
    where
      n = V.page_count w m
      current_page = unwrap $ V.page_number w

page_nav :: (V.Viewable v) => v -> Int -> Int -> Int -> [Html ()]
page_nav w n current_page i = atm:ps
    where
      pln = strong_ $ toHtml $ show i
      lnk = _at (V.url w) (show i)
      atm = if (i == current_page) then
                pln
            else
                lnk
      ps = if (n==i) then
               []
           else
               page_nav (V.next_page w) n current_page (i+1)

unwrap :: Maybe Int -> Int
unwrap Nothing = 1
unwrap (Just n) = n

-- Come back to this
sidebar :: (V.Viewable v) => v -> CB.ChromeBackEnd -> B.Model -> [B.Item] -> IO (Html ())
sidebar w cb b p =
    do { fc <- FC.build_collage $ CB.flickr_collage cb
       ; tc <- H.get $ CB.tag_cloud_holder cb
       ; soc <- SoC.get_content $ CB.soc cb
       ; return $ divid "sidebar" $ do
           sidebar_nav w b
           contents_block "sidebarcontents" w b p
           h3_ $ toHtml ("Tags" :: String)
           toHtmlRaw tc
           h3_ $ toHtml ("Stream Of Consciousness" :: String)
           toHtmlRaw soc
           h3_ $ toHtml ("Pictures" :: String)
           fc }

contents_block :: (V.Viewable v) => String -> v -> B.Model -> [B.Item] -> Html ()
contents_block s w b i | (V.kind w == V.Single) = mempty
                       | otherwise = do
                           h3_ $ toHtml $ concat [ "Page "
                                                 , maybe_page_number
                                                 , " Contents"]
                           div_ [class_ (T.pack s)] $
                               mconcat $ map (p_ . (contents_item b)) i
    where
      maybe_page_number = if (V.page_count w b) > 1 then
                              show . unwrap . V.page_number $ w
                          else
                              ""

contents_item :: B.Model -> B.Item -> Html ()
contents_item m i = do
    toHtml ((take 10 $ B.created i) ++ " > ")
    _a (B.permalink m i) (toHtmlRaw $ B.title i)

build_discoverable_feeds ::(V.Viewable v) => v -> Html ()
build_discoverable_feeds v = mconcat $ map disc_feed (V.discoverable_feeds v)

disc_feed :: F.DiscoverableFeed -> Html ()
disc_feed f = link_ [ rel_ "alternate"
                    , type_ "application/atom+xml"
                    , title_ (T.pack $ F.feed_title f)
                    , href_ (T.pack $ F.feed_url f) ]

-- Paging should occur before this function is applied.
renderer :: (V.Viewable v) => v -> CB.ChromeBackEnd -> B.Model -> [B.Item] -> IO (Html ())
renderer v _ _ [] = return (no_posts $ V.no_posts_message v)
renderer v cb m l = if (V.kind v == V.Single) then
                        render_post_detail cb m $ head l
                    else
                        return $ render_posts m l

no_posts :: String -> Html ()
no_posts s = p_ [class_ "no_posts"] $ toHtml s

-- | Render a detail view of a single post, i.e., the post body
-- along with the comments.
render_post_detail :: CB.ChromeBackEnd -> B.Model -> B.Item -> IO (Html ())
render_post_detail cb m i = do { dc <- D.get_chrome (CB.delicious_controller cb) m i
                               ; let tags = (render_tags i)
                               ; counts <- comment_count_and_hit_count cb m i
                               ; return $ render_ render_comments_as_text (do
                                   meta_heading
                                   tags
                                   _group dc
                                   counts) m i }

-- | Render a list of posts with the post bodies and comments as counts
-- only.
render_posts :: B.Model -> [B.Item] -> Html ()
render_posts m ii = mconcat $ map (render_ render_comments_as_count mempty m) ii

render_ :: (B.Model -> B.Item -> Html ()) -> Html () -> B.Model -> B.Item -> Html ()
render_ render_comments chrome m i = do
    div_ [class_ "entry_wrapper"] $ do
        post_heading m i
        render_body "entry" i
        chrome
    render_comments m i

meta_heading :: Html ()
meta_heading = h3_ [class_ "meta_heading"] $ toHtml ("Meta" :: String)

render_comments_as_count :: B.Model -> B.Item -> Html ()
render_comments_as_count m i = _group $ do
    _left $ fancy_comment_count m i
    _right $ add_comment_link m i

comment_or_comments :: B.Model -> B.Item -> String
comment_or_comments m i = case length $ B.children m i of
                            1 -> " comment"
                            _ -> " comments"

fancy_comment_count :: B.Model -> B.Item -> Html ()
fancy_comment_count m i = do
    img_ [ src_ "images/comments.png"
         , style_ "vertical-align: top;"
         , alt_ "(comment bubbles)" ]
    toHtml (" " :: String)
    span_ [class_ "comment_count"] $ toHtml $ show $ length $ B.children m i
    toHtml $ comment_or_comments m i

hit_count :: Int -> Html ()
hit_count i = do
    span_ [class_ "hit_count"] $ toHtml $ show i
    toHtml (" direct views" :: String)

comment_count_and_hit_count :: CB.ChromeBackEnd -> B.Model -> B.Item -> IO (Html ())
comment_count_and_hit_count cb m i = do { hc <- HitT.get_hits (CB.hit_tracker cb) i
                                        ; return $ _group $ do
                                            _left $ fancy_comment_count m i
                                            _right $ hit_count hc }

_left :: Html () -> Html ()
_left h = div_ [class_ "left"] h

_right :: Html () -> Html ()
_right h = div_ [class_ "right"] h

empty_right :: Html ()
empty_right = _right $ toHtml (" " :: String)

_group :: Html () -> Html ()
_group h = div_ [class_ "group"] h

comment_link :: B.Model -> B.Item -> Html ()
comment_link _ i = a_ [href_ (T.pack $ U.add_comment $ B.permatitle i), class_ "add_comment_link"] $
                       toHtml ("Add a comment." :: String)

add_comment_link :: B.Model -> B.Item -> Html ()
add_comment_link m i = do
    img_ [ src_ "images/comment_add.png"
         , style_ "vertical-align: top;"
         , alt_ "(new comment bubble)" ]
    toHtml (" " :: String)
    comment_link m i

render_comments_as_count' :: B.Model -> B.Item -> Html ()
render_comments_as_count' m i = p_ $ a_ [name_ "comments"] $ toHtml x
    where
      x = case (length $ B.children m i) of
            0 -> "No comments.  "
            1 -> "One comment.  "
            n -> ((show n) ++ " comments.  ")


-- Render a list of comments from the post
render_comments_as_text :: B.Model -> B.Item -> Html ()
render_comments_as_text m i = div_ [class_ "comments"] $ do
    comment_block
    comments_as_text
  where
    comment_block = _group $ do
        _left $ add_comment_link m i
        empty_right
    comments_as_text = case (length $ B.children m i) of
                         0 -> mempty
                         _ -> mconcat (map (render_comment_as_text m i) (B.children m i))

-- Render a single comment
render_comment_as_text :: B.Model -> B.Item -> B.Item -> Html ()
render_comment_as_text m px i = div_ [class_ "commentwrapper"] $ do
    comment_heading m px i
    render_body "comment" i

post_heading :: B.Model -> B.Item -> Html ()
post_heading m i = do
    h2_ $ _a (B.permalink m i) (toHtmlRaw $ B.title i)
    _group $ _right $ do
        post_author $ B.author i
        timestamp i

comment_heading :: B.Model -> B.Item -> B.Item -> Html ()
comment_heading model _ comment = p_ [class_ "commentattribution"] $ do
    toHtml ("Comment from " :: String)
    comment_author $ B.author comment
    timestamp comment
    toHtml (" " :: String)
    a_ [name_ (T.pack $ B.permatitle comment)] $ toHtml ("#" :: String)
    toHtml (" " :: String)
    _at (B.permalink model comment) "permalink"

timestamp :: B.Item -> Html ()
timestamp post = toHtml $ " @ " ++ (B.created post)

comment_author :: B.Author -> Html ()
comment_author (B.Author n (Just u) _ _) = em_ $ _at u n
comment_author (B.Author n Nothing _ _) = em_ $ toHtml n

post_author :: B.Author -> Html ()
post_author (B.Author n _ (Just e) True) = em_ $ _at ("mailto:" ++ e) n
post_author (B.Author n _ _ False) = em_ $ toHtml n


{- Come back and add comment count. -}
render_heading :: B.Model -> B.Item -> Html ()
render_heading m i = h2_ [class_ "title entry-title"] $
                         _at (B.permalink m i) (B.title i)

-- | Compute the link for a specific tag; uses the base URL for the app.
tag_link :: String -- ^ the tag
         -> String
tag_link tag = "/t/" ++ tag

-- | Render a single tag as a hyperlinked word
render_tag :: String -- ^ the tag name
           -> Html ()
render_tag tag = span_ [rel_ "tag"] $ do
    img_ [ src_ "images/tag.png"
         , alt_ "(tag)"]
    _a (tag_link tag) t
  where
    t = toHtml tag

-- | Render the tags from a post as Html
render_tags :: B.Item -- ^ the post
            -> Html ()
render_tags b | (B.tags b) == [] = p_ [class_ "tags"] $ toHtml ("No tags." :: String)
render_tags b = p_ [class_ "tags"] $ do
    toHtml ("Tags: " :: String)
    mconcat $ intersperse (toHtml (" " :: String)) (map render_tag (B.tags b))

unM :: Maybe String -> String
unM Nothing = error "Shouldn't try to unwrap Nothing..."
unM (Just s) = s

sub_for_nothing :: String -> Maybe String -> String
sub_for_nothing s Nothing = s
sub_for_nothing _ (Just t) = t

-- | Wrap a raw text post body in a @<div>@.
render_body :: String -- ^ the CSS class to apply
            -> B.Item -- ^ the item containing the body
            -> Html ()
render_body clazz b = div_ [class_ (T.pack clazz)] $ toHtmlRaw $ B.body b
