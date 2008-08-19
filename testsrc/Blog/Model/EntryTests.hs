module Blog.Model.EntryTests where

import qualified Blog.Model.Entry as B

import Data.List
import Test.HUnit

testBlogModel = TestList [ testPostInsertion
                         , testCommentInsertion ]

p1 = B.Item { B.internal_id = -1
            , B.kind = B.Post 
            , B.title = "Post 1"
            , B.summary = Nothing
            , B.body = "Post 1 body"
            , B.tags = ["a","b","c"]
            , B.uid = "uid://1"
            , B.permatitle = "post-1"
            , B.created = "2000-01-01T00:00:00Z"
            , B.updated = "2000-01-01T00:00:00Z"
            , B.author = B.default_author
            , B.visible = True
            , B.parent = Nothing }

p2 = p1 { B.internal_id = -1
        , B.title = "Post 2"
        , B.body = "Post 2 body"
        , B.tags = ["b","c","d"]
        , B.uid = "uid://2"
        , B.permatitle = "post-2"
        , B.created = "2000-01-02T00:00:00Z"
        , B.updated = "2000-01-02T00:00:00Z" }

p3 = p1 { B.internal_id = -1
        , B.title = "Post 3"
        , B.body = "Post 3 body"
        , B.tags = ["d","e","f"]
        , B.uid = "uid://3"
        , B.permatitle = "post-3"
        , B.created = "2000-01-03T00:00:00Z"
        , B.updated = "2000-01-03T00:00:00Z" }

testPostInsertion = test [
                    -- all_posts contains entries inserted 
                    (B.permatitle_exists m123 (B.permatitle p1)) ~? "p1 should be present by permatitle"
                    , (B.permatitle_exists m123 (B.permatitle p2)) ~? "p2 should be present by permatitle"
                    , (B.permatitle_exists m123 (B.permatitle p3)) ~? "p3 should be present by permatitle" 
                    -- all_posts is in reverse chronological order
                    , (map (B.permatitle) (B.all_posts m123)) ~?= ["post-3","post-2","post-1"]
                    -- all internal_ids were updated, i.e., no -1's
                    , not (-1 `elem` (map B.internal_id $ B.all_posts m123)) ~? "No internal ids should be -1 after insertion."
                    ]
    where
      m123 = snd . (ins p1) . (ins p2) . (ins p3) $ (p1, B.empty)
      ins = (flip (B.insert . snd))


c :: Int -> B.Item
c i = B.Item { B.internal_id = -1
             , B.kind = B.Comment 
             , B.title = ""
             , B.summary = Nothing
             , B.body = "Comment " ++ si ++ " body"
             , B.tags = []
             , B.uid = "comment://" ++ si
             , B.permatitle = "comment-" ++ si
             , B.created = "2000-0" ++ si ++ "-01T00:00:00Z"
             , B.updated = "2000-0" ++ si ++ "-01T00:00:00Z"
             , B.author = B.default_author
             , B.visible = True
             , B.parent = Just 0 }
      where 
        si = show i

testCommentInsertion = test [
                       -- all_posts contains comments inserted
                       "all comments present" ~: (sort . (map B.permatitle) $ B.all_comments mc) ~?= ["comment-1","comment-2","comment-3"]
                       -- comments in reverse chronological order as post items
                       , (map B.permatitle $ B.all_comments mc) ~?= ["comment-3","comment-2","comment-1"]
                       , (map B.permatitle $ B.all_items mc) ~?= ["comment-3","comment-2","comment-1","post-1"]
                       -- commentsi n chronological order as children
                       , (map B.permatitle $ B.children mc ip1) ~?= ["comment-1","comment-2","comment-3"]
                       -- all internal_ids were updated, i.e., no -1's
                       , (not (-1 `elem` (map B.internal_id $ B.all_items mc))) ~? "No internal ids should be -1 after insertion."
                       -- make sure that relative URLs are right
                       , (map (B.relative_url mc) (B.all_items mc)) ~?= [ "/post-1#comment-3"
                                                                        , "/post-1#comment-2"
                                                                        , "/post-1#comment-1"
                                                                        , "/post-1" ]
                       ]
    where
      ins = (flip (B.insert . snd))
      (ip1, mp1) = B.insert B.empty p1
      c1 = (c 1) { B.parent = Just $ B.internal_id ip1 }
      c2 = (c 2) { B.parent = Just $ B.internal_id ip1 }
      c3 = (c 3) { B.parent = Just $ B.internal_id ip1 }
      mc = snd . (ins c1) . (ins c3) . (ins c2) $ (p1,mp1)
      
