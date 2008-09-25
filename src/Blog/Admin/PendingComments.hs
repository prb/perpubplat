module Blog.Admin.PendingComments where

import qualified Blog.Model.CommentBodyParser as CBP
import qualified Blog.Model.Entry as B
import qualified Blog.Constants as C
import qualified Blog.FrontEnd.Urls as U
import Utilities

import Data.Maybe
import Blog.FrontEnd.ContentAtoms
import Text.XHtml.Strict

display_comments :: B.Model -> Int -> [B.Item] -> String
display_comments m page cmts
    = showHtml
      . concatHtml $ [ header << stylesheet
                     , body . (divid "container") . concatHtml $ [ navigation page cmts
                                                                 , nuke page chunk
                                                                 , concatHtml ( map (render m page) chunk ) ] ]
    where
      chunk = paginate C.comment_view_size page cmts
     

nuke :: Int -> [B.Item] -> Html
nuke _ [] = noHtml
nuke n items = ( p ! [ theclass "nuke_em" ] )
               $ nuke_comments_form (map (show . B.internal_id) items) n
 
navigation :: Int -> [B.Item] -> Html
navigation _ [] = (p ! [ theclass "comment_nav" ] )
                  << stringToHtml "There are no comments waiting for review."                               
navigation n cmts = ( p ! [ theclass "comment_nav" ] )
                    . concatHtml $ [ stringToHtml ( "There " ++ isare )
                                   , bold . stringToHtml . show . length $ cmts
                                   , stringToHtml ( plural ++ " waiting for review." )
                                   , br 
                                   , page_enumeration n cmts ]
    where
      isare = case length cmts of
                1 -> "is "
                _ -> "are "
      plural = case length cmts of
                 1 -> " comment "
                 _ -> " comments "

page_enumeration :: Int -> [B.Item] -> Html
page_enumeration _ [] = noHtml
page_enumeration n s | (1 == last_page C.comment_view_size s) = noHtml
                     | otherwise = concatHtml $ map (page_link n lp) [1..lp]
    where
      lp = last_page C.comment_view_size s

page_link :: Int -> Int -> Int -> Html
page_link i k j | (i == j)  = (bold . stringToHtml . show $ j)
                              +++ if (j==k) then
                                      noHtml
                                  else
                                      (stringToHtml " ")
                | otherwise = (_at (U.pending_comments $ Just j) (show j)) +++ (stringToHtml " ")

render :: B.Model -> Int -> B.Item -> Html
render m n i = (thediv ! [ theclass "comment_container" ])
               . (table ! [ theclass "comment_wrapper" ] ) . tr . concatHtml $
               [ ( td ! [ theclass "data_cell" ] )
                 . concatHtml $ [ ( p ! [ theclass "comment_preview_fields" ])
                                  . concatHtml $ [ render_author . B.author $ i
                                                 , br
                                                 , d . B.created $ i
                                                 , stringToHtml " / # "
                                                 , d . show . B.internal_id $ i
                                                 , stringToHtml " / re: "
                                                 , _a (parent_plink) (primHtml . B.title $ parent) ]
                                , (thediv ! [ theclass "comment_body_preview"])
                                  << ( CBP.convert_comment_body . B.body $ i ) ]
               , ( td ! [ theclass "action_cell" ] )
                 . concatHtml $ [ post_comment_form int_id n
                                , edit_comment_form int_id
                                , delete_comment_form int_id n ]
               ]
    where
      int_id = show . B.internal_id $ i
      parent = (B.item_by_id m) . fromJust . B.parent $ i
      parent_plink = B.permalink m parent
      d = bold . stringToHtml

post_comment_form :: String -> Int -> Html
post_comment_form int_id n = ( form ! [ identifier $ "post-" ++ int_id
                                      , method "post"
                                      , action . U.post_comment $ int_id] )
                             . concatHtml $ [ callback int_id n
                                            , input ! [ thetype "submit"
                                                      , value "Post"
                                                      , theclass "post_comment_submit" ] ]

delete_comment_form :: String -> Int -> Html
delete_comment_form int_id n = ( form ! [ identifier $ "delete-" ++ int_id
                                        , method "post"
                                        , action U.delete_comment] )
                               . concatHtml $ [ callback int_id n
                                              , input ! [ thetype "submit"
                                                        , value "Delete"
                                                        , theclass "delete_comment_submit" ] ]


nuke_comments_form :: [String] -> Int -> Html
nuke_comments_form int_ids n = ( form ! [ identifier $ "nuke"
                                        , method "post"
                                        , action U.delete_comments ] )
                               . concatHtml $ [ callbacks int_ids n
                                              , input ! [ thetype "submit"
                                                        , value "Nuke All On Page"
                                                        , theclass "nuke_comments_submit" ] ]

callbacks :: [String] -> Int -> Html
callbacks int_ids n = (hidden_int_ids int_ids)
                      +++ (hidden_page n)

hidden_int_id :: String -> Html
hidden_int_id int_id =  input ! [ thetype "hidden"
                                , name "id"
                                , value int_id ]

hidden_int_ids :: [String] -> Html
hidden_int_ids = concatHtml . map (\x -> input ! [ thetype "hidden"
                                             , name $ "id_" ++ x
                                             , value x ])

hidden_page :: Int -> Html
hidden_page n = input ! [ thetype "hidden"
                        , name "page"
                        , value (show n) ]

callback :: String -> Int -> Html
callback int_id n = (hidden_int_id int_id) 
                    +++ (hidden_page n)

edit_comment_form :: String -> Html
edit_comment_form int_id = ( form ! [ identifier $ "edit-" ++ int_id
                                    , method "get"
                                    , action . U.review_comment $ int_id ] )
                           << input ! [ thetype "submit"
                                      , value "Edit"
                                      , theclass "edit_comment_submit" ]

render_author :: B.Author -> Html
render_author a = concatHtml [ bold . stringToHtml . B.name $ a
                             , stringToHtml " / "
                             , bold . stringToHtml $ case (B.email a) of
                                                Nothing -> "<no email>"
                                                Just e -> e
                             , stringToHtml " / "
                             , case B.uri a of
                                 Nothing ->
                                     stringToHtml "<no URI>"
                                 Just u ->
                                     _a u (bold . stringToHtml $ u) ]