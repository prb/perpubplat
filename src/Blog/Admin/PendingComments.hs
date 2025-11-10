{-# LANGUAGE OverloadedStrings #-}
module Blog.Admin.PendingComments where

import qualified Blog.Model.CommentBodyParser as CBP
import qualified Blog.Model.Entry as B
import qualified Blog.Constants as C
import qualified Blog.FrontEnd.Urls as U
import Utilities

import Data.Maybe
import Blog.FrontEnd.ContentAtoms
import Lucid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

display_comments :: B.Model -> Int -> [B.Item] -> String
display_comments m page cmts =
    TL.unpack . renderText $ do
        head_ stylesheet
        body_ $ divid "container" $ do
            navigation page cmts
            nuke page chunk
            mconcat $ map (render m page) chunk
  where
    chunk = paginate C.comment_view_size page cmts

nuke :: Int -> [B.Item] -> Html ()
nuke _ [] = mempty
nuke n items = p_ [class_ "nuke_em"] $
    nuke_comments_form (map (show . B.internal_id) items) n

navigation :: Int -> [B.Item] -> Html ()
navigation _ [] = p_ [class_ "comment_nav"] $
    toHtml ("There are no comments waiting for review." :: String)
navigation n cmts = p_ [class_ "comment_nav"] $ do
    toHtml ( "There " ++ isare )
    strong_ $ toHtml $ show $ length cmts
    toHtml ( plural ++ " waiting for review." )
    br_ []
    page_enumeration n cmts
  where
    isare = case length cmts of
              1 -> "is "
              _ -> "are "
    plural = case length cmts of
               1 -> " comment "
               _ -> " comments "

page_enumeration :: Int -> [B.Item] -> Html ()
page_enumeration _ [] = mempty
page_enumeration n s | (1 == last_page C.comment_view_size s) = mempty
                     | otherwise = mconcat $ map (page_link n lp) [1..lp]
    where
      lp = last_page C.comment_view_size s

page_link :: Int -> Int -> Int -> Html ()
page_link i k j | (i == j)  = do
                      strong_ $ toHtml $ show j
                      if (j==k) then mempty else toHtml (" " :: String)
                | otherwise = do
                      _at (U.pending_comments $ Just j) (show j)
                      toHtml (" " :: String)

render :: B.Model -> Int -> B.Item -> Html ()
render m n i = div_ [class_ "comment_container"] $
    table_ [class_ "comment_wrapper"] $ tr_ $ do
        td_ [class_ "data_cell"] $ do
            p_ [class_ "comment_preview_fields"] $ do
                render_author $ B.author i
                br_ []
                d $ B.created i
                toHtml (" / # " :: String)
                d $ show $ B.internal_id i
                toHtml (" / re: " :: String)
                _a parent_plink (toHtmlRaw $ B.title parent)
            div_ [class_ "comment_body_preview"] $
                CBP.convert_comment_body $ B.body i
        td_ [class_ "action_cell"] $ do
            post_comment_form int_id n
            edit_comment_form int_id
            delete_comment_form int_id n
  where
    int_id = show . B.internal_id $ i
    parent = (B.item_by_id m) . fromJust . B.parent $ i
    parent_plink = B.permalink m parent
    d = strong_ . toHtml

post_comment_form :: String -> Int -> Html ()
post_comment_form int_id n = form_ [ id_ (T.pack $ "post-" ++ int_id)
                                   , method_ "post"
                                   , action_ (T.pack $ U.post_comment int_id) ] $ do
    callback int_id n
    input_ [ type_ "submit"
           , value_ "Post"
           , class_ "post_comment_submit" ]

delete_comment_form :: String -> Int -> Html ()
delete_comment_form int_id n = form_ [ id_ (T.pack $ "delete-" ++ int_id)
                                     , method_ "post"
                                     , action_ (T.pack U.delete_comment) ] $ do
    callback int_id n
    input_ [ type_ "submit"
           , value_ "Delete"
           , class_ "delete_comment_submit" ]

nuke_comments_form :: [String] -> Int -> Html ()
nuke_comments_form int_ids n = form_ [ id_ "nuke"
                                     , method_ "post"
                                     , action_ (T.pack U.delete_comments) ] $ do
    callbacks int_ids n
    input_ [ type_ "submit"
           , value_ "Nuke All On Page"
           , class_ "nuke_comments_submit" ]

callbacks :: [String] -> Int -> Html ()
callbacks int_ids n = do
    hidden_int_ids int_ids
    hidden_page n

hidden_int_id :: String -> Html ()
hidden_int_id int_id = input_ [ type_ "hidden"
                               , name_ "id"
                               , value_ (T.pack int_id) ]

hidden_int_ids :: [String] -> Html ()
hidden_int_ids = mconcat . map (\x -> input_ [ type_ "hidden"
                                              , name_ (T.pack $ "id_" ++ x)
                                              , value_ (T.pack x) ])

hidden_page :: Int -> Html ()
hidden_page n = input_ [ type_ "hidden"
                       , name_ "page"
                       , value_ (T.pack $ show n) ]

callback :: String -> Int -> Html ()
callback int_id n = do
    hidden_int_id int_id
    hidden_page n

edit_comment_form :: String -> Html ()
edit_comment_form int_id = form_ [ id_ (T.pack $ "edit-" ++ int_id)
                                 , method_ "get"
                                 , action_ (T.pack $ U.review_comment int_id) ] $
    input_ [ type_ "submit"
           , value_ "Edit"
           , class_ "edit_comment_submit" ]

render_author :: B.Author -> Html ()
render_author a = do
    strong_ $ toHtml $ B.name a
    toHtml (" / " :: String)
    strong_ $ toHtml $ case (B.email a) of
                         Nothing -> "<no email>"
                         Just e -> e
    toHtml (" / " :: String)
    case B.uri a of
        Nothing ->
            toHtml ("<no URI>" :: String)
        Just u ->
            _a u (strong_ $ toHtml u)