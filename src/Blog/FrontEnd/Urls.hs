module Blog.FrontEnd.Urls where

import Blog.Constants as C
import List (intersperse)

all_posts :: String
all_posts = C.base_url ++ "/articles"

posts_by_tag :: String -> String
posts_by_tag t = "/t/" ++ t

posts_by_tags :: [String] -> String
posts_by_tags t = "/t/" ++ (tags_fragment t)

post :: String -> String
post p = C.base_url ++ "/p" ++ p

add_comment :: String -> String
add_comment p = C.base_url ++ "/c/add-comment/" ++ p

pending_comments :: Maybe Int -> String
pending_comments Nothing = C.base_url ++ "/z/review-comments"
pending_comments (Just n) = C.base_url ++ "/z/review-comments/p/" ++ (show n)

add_comment_target :: String -> String
add_comment_target p = C.base_url ++ "/e/add-comment/" ++ p

post_comment :: String -> String
post_comment _ = C.base_url ++ "/x/post-comment"

review_comment :: String -> String
review_comment i = C.base_url ++ "/z/review-comment/" ++ i

edit_comment_target :: Int -> String
edit_comment_target i = C.base_url ++ "/x/edit-comment/" ++ (show i)

delete_comment :: String
delete_comment = C.base_url ++ "/x/delete-comment"

delete_comments :: String
delete_comments = C.base_url ++ "/x/delete-comments" 

comments :: String -> String
comments p = p ++ "#comments"

tags_fragment :: [String] -> String
tags_fragment = concat . (intersperse ",")