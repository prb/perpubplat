-- | Data structures for an item (post or comment) and the
-- overall structure in terms of parents and children.
module Blog.Model.Entry where

import qualified Blog.FrontEnd.Urls as U
import Utilities
import qualified Blog.Constants as C

import Data.Maybe
import Data.List ( sortBy, isPrefixOf, intersperse)
import qualified Data.Map as M
import Data.Map ( (!) )

type ISO8601DatetimeString = String
type XhtmlString = String

-- | Overall data model for the runtime.  
data Model = Model { -- | 
                     by_permatitle :: M.Map String Item,
                     by_int_id :: M.Map Int Item,
                     child_map :: M.Map Int [Int],
                     all_items :: [Item],
                     next_id :: Int }

empty :: Model
empty = Model M.empty M.empty M.empty [] 0

data Kind = Post | Comment | Trackback
            deriving (Show, Read, Eq)

build_model :: [Item] -> Model
build_model [] = empty
build_model items = Model (map_by permatitle sorted_items)
                    bid                    
                    (build_child_map sorted_items)
                    (sorted_items)
                    (n+1)
    where
      sorted_items = sort_by_created_reverse items
      bid = (map_by internal_id sorted_items)
      n = fst . M.findMax $ bid

build_child_map :: [Item] -> M.Map Int [Int]
build_child_map i = build_child_map_ (M.fromList $ (map (\x -> (internal_id x,[])) i)) i

-- Constructed to take advantage of the input being in sorted order.
build_child_map_ :: M.Map Int [Int] -> [Item] -> M.Map Int [Int]
build_child_map_ m [] = m
build_child_map_ m (i:is) = if (parent i == Nothing) then
                                build_child_map_ m is
                            else
                                build_child_map_ (M.insertWith (++) (unwrap $ parent i) [internal_id i] m) is

-- | Insert an item, presuming that all of its data other than
-- internal identifier have been correctly set.
insert :: Model -> Item -> (Item,Model)
insert m i = (i', m { by_permatitle = M.insert (permatitle i') i' $ by_permatitle m
                    , by_int_id = M.insert n i' $ by_int_id m
                    , child_map = M.insert (internal_id i') [] $
                                  case parent i of
                                    Nothing ->
                                        child_map m
                                    (Just p_id) ->
                                        M.insert p_id (insert_comment_ m (item_by_id m p_id) i') $ child_map m
                    , all_items = insert_ after (all_items m) i'
                    , next_id = n + 1 } )
    where
      n = next_id m
      i' = i { internal_id = n }

insert_comment_ :: Model -> Item -> Item -> [Int]
insert_comment_ m p c = map internal_id (insert_ before (children m p) c)

insert_ :: (Item -> Item -> Bool) -> [Item] -> Item -> [Item]
insert_ _ [] y = [y]
insert_ o s@(x:xs) y = if (x `o` y) then
                           (x:(insert_ o xs y))
                       else
                           (y:s)

after :: Item -> Item -> Bool
after a b = (created a) > (created b)

before :: Item -> Item -> Bool
before a b = (created a) < (created b)

-- | Apply a structure-preserving function, i.e., one that does not
-- change parent/child relationships or ids, to a specific item.
alter :: (Item -> Item) -> Model -> Item -> IO Model
alter f m i = do { ts <- now
                 ; let i' = (f i) { updated = ts }
                 ; return $ m { by_permatitle = M.insert (permatitle i') i' $ by_permatitle m
                              , by_int_id = M.insert (internal_id i') i' $ by_int_id m
                              , child_map = if (parent i == Nothing) then
                                                child_map m
                                            else
                                                M.insert p_id resort_siblings $ child_map m
                              , all_items = insert_ after all_but i' } }
    where
      not_i = \item -> (internal_id item) /= (internal_id i)
      all_but = filter not_i $ all_items m
      p_id = unwrap $ parent i
      p = item_by_id m p_id
      resort_siblings = map internal_id (insert_ before (filter not_i $ children m p) i)

cloak :: Model -> Item -> IO Model
cloak = alter (\i -> i { visible = False })

uncloak :: Model -> Item -> IO Model
uncloak = alter (\i -> i { visible = True })

permatitle_exists :: Model -> String -> Bool
permatitle_exists = (flip M.member) . by_permatitle

max_id :: Model -> Int
max_id = fst . M.findMax . by_int_id

post_by_permatitle :: Model -> String -> Item
post_by_permatitle = (!) . by_permatitle

maybe_post_by_permatitle :: Model -> String -> Maybe Item
maybe_post_by_permatitle = (flip M.lookup) . by_permatitle

item_by_id :: Model -> Int -> Item
item_by_id = (!) . by_int_id

children :: Model -> Item -> [Item]
children m i = map (item_by_id m) ((child_map m) ! (internal_id i))

unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap Nothing = error "Can't unwrap nothing!"

data Author = Author { name :: String,
		       uri :: Maybe String,
		       email :: Maybe String,
		       show_email :: Bool
		     }
	      deriving ( Show,Read,Eq )

-- | General purpose runtime data structure for holding a post or
-- comment.  For a comment, a number of the fields will be ignored
-- (e.g., comments and tags) until/if the presentation and syndication
-- system gets fancier.
data Item = Item { -- | an internal unique number for this post
                   internal_id :: Int,
                   -- | the kind of item that this represents
                   kind :: Kind,
                   -- | the title of the post, as it should be rendered on
                   -- the web or inserted in an Atom feed; this should be a
                   -- valid XHTML fragment.
                   title :: XhtmlString,
                   -- | the summary of the post, as it should be rendered on
                   -- the web or intersted into an Atom feed; this should be
                   -- a valid XHTML fragment.
	           summary :: Maybe XhtmlString,
                   -- | the body of the post as an XHTML fragment.  This
                   -- will be wrapped in an XHTML @<div>@ when rendered on
                   -- the web or in a feed.
	           body :: XhtmlString,
                   -- | tags for the post, if any, expected to be in
                   -- alphabetical order and consisting of letters, digits,
                   -- dashes, and/or underscores.
	           tags :: [String],
                   -- | a generated UID for the post; this is expected to be
                   -- suitable for use as an Atom GUID.  The expectation is
                   -- that it will be supplied by the implementation when
                   -- the post is ingested.
	           uid :: String,
                   -- | a permanent title for the item, consisting of only
                   -- lowercase letters, digits, and dashes.
                   permatitle :: String,
                   -- | the timestamp, as an ISO8601 datetime, when the post
                   -- came into being.  This is never blank and would be
                   -- supplied by the implementation when the post is
                   -- ingested.
                   created :: ISO8601DatetimeString,
                   -- | the timestamp, as an ISO8601 datetime, when the post
                   -- was updated.  Initially, this is equal to the value of
                   -- the 'created' field.
	           updated :: ISO8601DatetimeString,
                   -- | the author of the post, expected to be hardwired to
                   -- the author of the blog
	           author :: Author,
                   -- | whether or not the item is to be displayed.
	           visible :: Bool,
                   -- | this item's parent, if any.
                   parent :: Maybe Int
	         }
            deriving ( Show, Read, Eq )

-- | Compute a permalink for the item relative to the supplied base URL.
permalink :: Model
          -> Item -- ^ the item
          -> String
permalink m i = U.post (relative_url m i)

relative_url :: Model -> Item -> String
relative_url m = _form_permalink . (ancestors m)

_form_permalink :: [Item] -> String
_form_permalink [] = ""
_form_permalink [i] =  let s = permatitle i in 
                       if (kind i == Post) then
                           "/" ++ s
                       else
                           "#" ++ s
_form_permalink (i:is) = if (kind i == Post) then
                            ("/" ++ permatitle i) ++ (_form_permalink is)
                        else 
                            (_form_permalink is)


ancestor_path :: Model -> Item -> String
ancestor_path m i = concat . (intersperse "/") . (map permatitle) $ ancestors m i

ancestors :: Model -> Item -> [Item]
ancestors m i = ancestors_ m [] (Just $ internal_id i)

ancestors_ :: Model -> [Item] -> Maybe Int -> [Item]
ancestors_ _ is Nothing = is
ancestors_ m is (Just i) = ancestors_ m (i':is) (parent i')
    where
      i' = item_by_id m i

lastUpdated :: [Item] -> ISO8601DatetimeString
lastUpdated ps = maximum (map updated ps)

drop_invisible :: [Item] -> [Item]
drop_invisible = filter visible

sort_by_created :: [Item] -> [Item]
sort_by_created = sortBy created_sort

created_sort :: Item -> Item -> Ordering
created_sort a b = compare (created a) (created b)

sort_by_created_reverse :: [Item] -> [Item]
sort_by_created_reverse = sortBy created_sort_reverse

created_sort_reverse :: Item -> Item -> Ordering
created_sort_reverse a b = compare (created b) (created a)

-- | Filter a list of items according to a date fragment
date_fragment_filter_ :: ISO8601DatetimeString -> [Item] -> [Item]
date_fragment_filter_ s = filter ((s `isPrefixOf`) . created)

-- | Filter a list of posts for those made in a specific year.
year_filter :: Int -- ^ year
            -> [Item] -> [Item]
year_filter y = date_fragment_filter_ $ show y

-- | Filter a list of posts for those made in a specific month.              
month_filter :: Int -- ^ year
             -> Int -- ^ month
             -> [Item] -> [Item]
month_filter y m | (0 < m) && (m < 13) = date_fragment_filter_ ((show y) ++ (pad_ m))
                 | otherwise = const []

-- | Filter a list of posts for those made on a specific day
day_filter :: Int -- ^ year
           -> Int -- ^ month
           -> Int -- ^ day
           -> [Item] -> [Item]
day_filter y m d = date_fragment_filter_ ((show y) ++ (pad_ m) ++ (pad_ d))

-- | Utility function to zero pad months and days in date expressions.
pad_ :: Int -> String
pad_ i | i < 10 = "-0" ++ (show i)
       | otherwise = ('-':(show i))
              
-- to do: make this faster using the sortedness.
tags_filter :: [String] -> [Item] -> [Item]
tags_filter t p = foldl (flip ($)) p (map tag_filter t)

tag_filter :: String -> [Item] -> [Item]
tag_filter t = filter ((t `elem`) . tags) 

plink_filterf :: String -> Item -> Bool
plink_filterf = flip $ (==) . permatitle

plink_filter :: String -> [Item] -> [Item]
plink_filter = filter . plink_filterf

ymd_plink_finder :: Int -> Int -> Int -> String -> [Item] -> [Item]
ymd_plink_finder y m d t = (plink_filter t) . (day_filter y m d)

all_posts :: Model -> [Item]
all_posts = (filter (\x -> Post == kind x)) . all_items

all_comments :: Model -> [Item]
all_comments = (filter (\x -> Comment == kind x)) . all_items

flatten :: Model -> [Item] -> [Item]
flatten m = flatten_ (children m)

flatten_ :: (a -> [a]) -> [a] -> [a]
flatten_ _ [] = []
flatten_ f (i:is) = (i:(flatten_ f (f i))) ++ (flatten_ f is)

concat_comments :: Model -> [Item] -> [Item]
concat_comments m = (foldr (++) []) . (map $ children m)

(</>) :: String -> String -> String
s </> t = s ++ ('/':t)

to_string :: Item -> String
to_string i = concat [metadata i, "\n", body_block i, "\n", summary_block i]

metadata :: Item -> String
metadata i = unlines $ apply i [ ("internal_id",show . internal_id),
                                 ("parent", show . parent),
                                 ("title",title),
                                 ("tags",show_no_quotes . tags),
                                 ("permatitle",permatitle),
                                 ("kind",show . kind),
                                 ("uid",uid),
                                 ("created",created),
                                 ("updated",updated),
                                 ("author",show . author),
                                 ("visible",show . visible) ]

show_no_quotes :: [String] -> String
show_no_quotes = concat . (intersperse ", ")

apply :: Item -> [(String,(Item -> String))] -> [String]
apply _ [] = []
apply i (x:xs) = ((concat [fst x, ": ", (snd x) i]) : (apply i xs))

body_block :: Item -> String
body_block i = concat ["--- START BODY ---\n",
                       (body i),
                       "\n--- END BODY ---\n"]

summary_block :: Item -> String
summary_block i | summary i == Nothing = ""
                | otherwise = concat ["--- START SUMMARY ---\n",
                                      (unwrap $ summary i),
                                      "\n--- END SUMMARY ---\n"]

default_author :: Author
default_author = Author C.author_name C.author_uri C.author_email True