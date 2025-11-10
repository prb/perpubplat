-- | Module for presentation of entries and lists of entries as XHTML
-- fragments and/or pages, as appropriate.
module Blog.FrontEnd.Presentation where

import Text.XHtml.Strict
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
             ; return . UTF8.encodeString . showHtml . concatHtml $
                          [ topmatter v
                          , body . (divid "container") . concatHtml $
                                     [ heading
                                     , divid "posts" $ phunk +++ bottom_nav v m
                                     , sb
                                     , footer ] ] }
    where
      posts = (V.lens v) m
      paged_posts = (V.page v) $ posts

topmatter :: (V.Viewable v) => v -> Html
topmatter v = header . concatHtml $ [ thetitle . stringToHtml $ V.title v
                                    , base_url
                                    , build_discoverable_feeds v
                                    , stylesheet
                                    , robots ]
    where
      robots = if (V.kind v == V.Single) then
                   meta ! [ name "ROBOTS", content "INDEX, FOLLOW"]
               else
                   meta ! [ name "ROBOTS", content "NOINDEX, FOLLOW"]

nav_blurb :: (V.Viewable v) => v -> B.Model -> Html
nav_blurb w m = concatHtml [ bold . stringToHtml $ V.title w
                           , stringToHtml " contains "
                           , bold . stringToHtml . show $ length ((V.lens w) m)
                           , stringToHtml" items in "
                           , bold . stringToHtml . show $ V.page_count w m
                           , stringToHtml " pages of "
                           , bold . stringToHtml . show $ V.page_size w
                           , stringToHtml " items each: " ]

bottom_nav :: (V.Viewable v) => v -> B.Model -> Html
bottom_nav w m | (V.kind w == V.Single) || ((V.page_count w m) == 1) = noHtml
               | otherwise = divid "bottomnav" . p . concatHtml $ [ nav_blurb w m
                                                                  , br
                                                                  , pages w m ]

sidebar_nav :: (V.Viewable v) => v -> B.Model -> Html
sidebar_nav w m | (V.kind w == V.Single) || ((V.page_count w m) == 1) = noHtml
                | otherwise = (h3 << stringToHtml "Navigation")
                              +++ ( p ! [ theclass "navigation" ] << ( concatHtml $ [ nav_blurb w m
                                                                                    , br
                                                                                    , pages w m ] ) )

pages :: (V.Viewable v) => v -> B.Model -> Html
pages w m = concatHtml
            $ (intersperse $ stringToHtml " ")
            $ page_nav (V.first_page w) n current_page 1
    where
      n = V.page_count w m
      current_page = unwrap $ V.page_number w

page_nav :: (V.Viewable v) => v -> Int -> Int -> Int -> [Html]
page_nav w n current_page i = atm:ps
    where
      pln = bold . stringToHtml $ show i
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
sidebar :: (V.Viewable v) => v -> CB.ChromeBackEnd -> B.Model -> [B.Item] -> IO Html
sidebar w cb b p =
    do { fc <- FC.build_collage $ CB.flickr_collage cb
       ; tc <- H.get $ CB.tag_cloud_holder cb
       ; soc <- SoC.get_content $ CB.soc cb
       ; return $ (divid "sidebar") . concatHtml $ 
         [ sidebar_nav w b
         , contents_block "sidebarcontents" w b p 
         , h3 $ stringToHtml "Tags"
         , primHtml tc
         , h3 $ stringToHtml "Stream Of Consciousness"
         , primHtml soc
         , h3 $ stringToHtml "Pictures"
         , fc ] }
                          
contents_block :: (V.Viewable v) => String -> v -> B.Model -> [B.Item] -> Html
contents_block s w b i | (V.kind w == V.Single) = noHtml
                       | otherwise = concatHtml [ h3 . stringToHtml $ concat [ "Page "
                                                                             , maybe_page_number
                                                                             , " Contents"]
                                                                                              
                                                , thediv ! [ theclass s ] << 
                                                  ( concatHtml . (map (p . (contents_item b))) $ i) ]
    where
      maybe_page_number = if (V.page_count w b) > 1 then
                              show . unwrap . V.page_number $ w
                          else 
                              ""

contents_item :: B.Model -> B.Item -> Html
contents_item m i = stringToHtml ((take 10 $ B.created i) ++ " > ") +++ _a (B.permalink m i) (primHtml $ B.title i)

build_discoverable_feeds ::(V.Viewable v) => v -> Html
build_discoverable_feeds v = concatHtml $ map disc_feed (V.discoverable_feeds v)

disc_feed :: F.DiscoverableFeed -> Html
disc_feed f = thelink ! [ rel "alternate",
                          thetype "application/atom+xml",
                          title $ F.feed_title f,
                          href $ F.feed_url f ] << noHtml

-- Paging should occur before this function is applied.
renderer :: (V.Viewable v) => v -> CB.ChromeBackEnd -> B.Model -> [B.Item] -> IO Html
renderer v _ _ [] = return (no_posts $ V.no_posts_message v)
renderer v cb m l = if (V.kind v == V.Single) then
                        render_post_detail cb m $ head l
                    else
                        return $ render_posts m l
    
no_posts :: String -> Html
no_posts s = p ! [ theclass "no_posts" ] << (stringToHtml s)

-- | Render a detail view of a single post, i.e., the post body
-- along with the comments.
render_post_detail :: CB.ChromeBackEnd -> B.Model -> B.Item -> IO Html
render_post_detail cb m i = do { dc <- D.get_chrome (CB.delicious_controller cb) m i
                               ; let tags = (render_tags i)
                               ; counts <- comment_count_and_hit_count cb m i
                               ; return $ render_ render_comments_as_text (concatHtml [ meta_heading
                                                                                      , tags
                                                                                      , _group dc
                                                                                      , counts]) m i }

-- | Render a list of posts with the post bodies and comments as counts
-- only.
render_posts :: B.Model -> [B.Item] -> Html
render_posts m ii = concatHtml $ map (render_ render_comments_as_count noHtml m) ii

render_ :: (B.Model -> B.Item -> Html) -> Html -> B.Model -> B.Item -> Html
render_ render_comments chrome m i
    = ( thediv ! [  theclass "entry_wrapper" ] $ concatHtml [ post_heading m i
                                                            , render_body "entry" i
                                                            , chrome ]
         )
      +++ (render_comments m i)

meta_heading :: Html
meta_heading = h3 ! [ theclass "meta_heading" ] << stringToHtml "Meta"

render_comments_as_count :: B.Model -> B.Item -> Html
render_comments_as_count m i = _group $ concatHtml [ _left $ fancy_comment_count m i
                                                   , _right $ add_comment_link m i ]

comment_or_comments :: B.Model -> B.Item -> String
comment_or_comments m i = case length $ B.children m i of
                            1 -> " comment"
                            _ -> " comments"

fancy_comment_count :: B.Model -> B.Item -> Html
fancy_comment_count m i = concatHtml [ image ! [ src "images/comments.png"
                                               , thestyle "vertical-align: top;"
                                               , alt "(comment bubbles)" ]
                                     , stringToHtml " "
                                     , thespan ! [ theclass "comment_count" ] << (show . length . (B.children m) $ i)
                                     , stringToHtml $ comment_or_comments m i ]

hit_count :: Int -> Html
hit_count i = concatHtml [ thespan ! [ theclass "hit_count" ] << (show i)
                         , stringToHtml $ " direct views" ]

comment_count_and_hit_count :: CB.ChromeBackEnd -> B.Model -> B.Item -> IO Html
comment_count_and_hit_count cb m i = do { hc <- HitT.get_hits (CB.hit_tracker cb) i
                                        ; return $ _group . concatHtml
                                                     $ [ _left $ fancy_comment_count m i
                                                       , _right $ hit_count hc ] }

_left :: Html -> Html
_left h = thediv ! [ theclass "left" ] << h

_right :: Html -> Html
_right h = thediv ! [ theclass "right" ] << h

empty_right :: Html
empty_right = _right $ stringToHtml " "

_group :: Html -> Html
_group h = thediv ! [ theclass "group" ] << h

comment_link :: B.Model -> B.Item -> Html
comment_link _ i = _at (U.add_comment $ B.permatitle i) "Add a comment." ! [ theclass "add_comment_link" ]

add_comment_link :: B.Model -> B.Item -> Html
add_comment_link m i = concatHtml [ image ! [ src "images/comment_add.png"
                                            , thestyle "vertical-align: top;"
                                            , alt "(new comment bubble)" ]
                                  , stringToHtml " "
                                  , comment_link m i]

render_comments_as_count' :: B.Model -> B.Item -> Html
render_comments_as_count' m i = p << anchor ! [ name "comments" ] << stringToHtml x
    where
      x = case (length $ B.children m i) of
            0 -> "No comments.  "
            1 -> "One comment.  "
            n -> ((show n) ++ " comments.  ")


-- Render a list of comments from the post
render_comments_as_text :: B.Model -> B.Item -> Html
render_comments_as_text m i
    = ( thediv ! [ theclass "comments" ] )
      $ comment_block +++ comments_as_text
    where
      comment_block = _group $ concatHtml [ _left $ add_comment_link m i
                                          , empty_right ]
      comments_as_text = case (length $ B.children m i) of
                           0 -> noHtml
                           _ -> concatHtml (map (render_comment_as_text m i) (B.children m i))
                                       
-- Render a single comment
render_comment_as_text :: B.Model -> B.Item -> B.Item -> Html
render_comment_as_text m px i  =  thediv ( (comment_heading m px i)
                                           +++ (render_body "comment" i) )
                                       ! [ theclass "commentwrapper" ]

post_heading :: B.Model -> B.Item -> Html
post_heading m i = concatHtml [ h2 (_a (B.permalink m i) (primHtml$B.title i))
                              , _group . _right $ (( post_author$B.author i)
                                                   +++ (timestamp i)) ]

comment_heading :: B.Model -> B.Item -> B.Item -> Html
comment_heading model _ comment
    = p ( (stringToHtml "Comment from ")
          +++ (comment_author$B.author comment)
          +++ (timestamp comment)
          +++ (stringToHtml " ")
          +++ (anchor ! [ name (B.permatitle comment)] << stringToHtml "#")
          +++ (stringToHtml " ")
          +++ (_at (B.permalink model comment) "permalink"))
      ! [ theclass "commentattribution" ]

timestamp :: B.Item -> Html
timestamp post = stringToHtml( " @ " ++ (B.created post))

comment_author :: B.Author -> Html
comment_author (B.Author n (Just u) _ _) = emphasize ( _at u n )
comment_author (B.Author n Nothing _ _) = emphasize (stringToHtml n)

post_author :: B.Author -> Html
post_author (B.Author n _ (Just e) True) = emphasize (_at ("mailto:" ++ e) n)
post_author (B.Author n _ _ False) = emphasize (stringToHtml n)


{- Come back and add comment count. -}
render_heading :: B.Model -> B.Item -> Html
render_heading m i = h2 ( _at (B.permalink m i) (B.title i) )
                     ! [ theclass "title entry-title" ]

-- | Compute the link for a specific tag; uses the base URL for the app.
tag_link :: String -- ^ the tag
         -> String
tag_link tag = "/t/" ++ tag

-- | Render a single tag as a hyperlinked word
render_tag :: String -- ^ the tag name
           -> Html
render_tag tag = image ! [ src "images/tag.png"
                         , alt "(tag)"] +++ (_a (tag_link tag) t)
                 ! [ rel "tag" ]
    where
      t = stringToHtml tag

-- | Render the tags from a post as Html
render_tags :: B.Item -- ^ the post
            -> Html
render_tags b | (B.tags b) == [] = p ( stringToHtml "No tags." )
                                   ! [ theclass "tags" ]
render_tags b = p ( (stringToHtml "Tags: ")
                    +++ concatHtml ( intersperse (stringToHtml " ")
                                     (map render_tag ( B.tags b)) ) )
                ! [ theclass "tags" ]

unM :: Maybe String -> String
unM Nothing = error "Shouldn't try to unwrap Nothing..."
unM (Just s) = s

sub_for_nothing :: String -> Maybe String -> String
sub_for_nothing s Nothing = s
sub_for_nothing _ (Just t) = t

-- | Wrap a raw text post body in a @<div>@.
render_body :: String -- ^ the CSS class to apply
            -> B.Item -- ^ the item containing the body
            -> Html
render_body clazz b = thediv ( primHtml$B.body b )
                                  ! [ theclass clazz ]
