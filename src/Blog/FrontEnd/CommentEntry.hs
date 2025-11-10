{-# LANGUAGE OverloadedStrings #-}
module Blog.FrontEnd.CommentEntry where

import qualified Blog.Model.Entry as B
import qualified Blog.Model.CommentForm as CF
import Blog.FrontEnd.ContentAtoms

import Lucid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

{-

Design for comments:

- Unique comment submission URIs: Open a new page for the form to
  submit a comment, create a submission URI that depends on the page.
  This is a substitute for session state.  The form processor is
  expected to compare the submission URI with a value sent down in the
  form.

- Allow editing a comment via the back button?

- Allow no-moderation posting of a comment if we've seen the email
  address and URI before.

- Markup Approach 1 - Minimal HTML syntax: Support <code>,
  <blockquote>, <pre>, and <a>.  Automatically turn linebreaks into
  <p>...</p> pairs.  Automatically escape entities, i.e., turn every &
  into &amp;. 

- Markup Approach 2 - Macro syntax: Steal Reddit's.

- Markup Approach 3 - Plain text only.


-}

comment_form :: B.Model -> B.Item -> String -> Maybe CF.CommentForm -> String
comment_form m i targ cf =
    TL.unpack . renderText $ do
        topmatter ( "Comment on " ++ B.permatitle i )
        body_ $ divid "container" $ do
            heading
            divid "comment_entry" $ do
                h3_ $ do
                    toHtml ("Comment on " :: String)
                    _a plink (toHtmlRaw $ B.title i)
                build_form targ plink cf
            comment_sidebar
            footer
  where
    plink = B.permalink m i


build_form :: String -> String -> Maybe CF.CommentForm -> Html ()
build_form targ plink cf = form_ [action_ (T.pack targ), method_ "post"] $ do
    textarea_ [ name_ "commentBody"
              , class_ "body_entry"
              , rows_ "10" ] $ toHtml $ CF.value bod
    case bod of
        (CF.Field _ _ (Just m)) ->
            p_ [class_ "comment_datum"] $
                span_ [class_ "required"] $ toHtml m
        _ ->
            mempty
    inp "Name" True "authorName" nam
    inp "Email (will not be published)" True "authorEmail" email
    inp "URL" True "authorUri" uri
    input_ [ class_ "comment_submit"
           , type_ "submit"
           , value_ "Post Comment" ]
    toHtml (" or just go " :: String)
    _at plink "back"
    toHtml ("." :: String)
  where
    bod = case cf of
            (Just c) -> CF.body c
            _ -> CF.Field "body" "" Nothing
    nam = case cf of
            (Just c) -> CF.authorName c
            _ -> CF.Field "name" "" Nothing
    uri = case cf of
            (Just c) -> CF.authorUri c
            _ -> CF.Field "URL" "" Nothing
    email = case cf of
              (Just c) -> CF.authorEmail c
              _ -> CF.Field "email" "" Nothing

topmatter :: String -> Html ()
topmatter tit = head_ $ do
    title_ $ toHtml tit
    base_url
    stylesheet
    robots

inp :: String -> Bool -> String -> CF.Field -> Html ()
inp f r n v = p_ [class_ "comment_datum"] $ do
    input_ [ name_ (T.pack n)
           , class_ "datum_entry"
           , value_ (T.pack $ CF.value v) ]
    if r
        then span_ [class_ "required"] $ toHtml ("*" :: String)
        else mempty
    toHtml (" " ++ f)
    case v of
        (CF.Field _ _ Nothing) ->
            mempty
        (CF.Field _ _ (Just m)) -> do
            br_ []
            span_ [class_ "required"] $ toHtml m

robots :: Html ()
robots = meta_ [name_ "ROBOTS", content_ "NOINDEX, FOLLOW"]

comment_sidebar :: Html ()
comment_sidebar = divid "sidebar" $ do
    h3_ $ toHtml ("Comment Formatting" :: String)
    formatting_help
    h3_ $ toHtml ("Comment Policies" :: String)
    policy_ "All comments are moderated to avoid spam, so your comment won't appear until after I've had a chance to look at it.  Once approved, comments are displayed in the order received."
    policy_ "You can find out when your comment appears and track other comments on this entry by subscribing to the entry's comment feed."
    policy_ "I'll post pretty much anything that's not purely inflammatory, but I do reserve the right to edit comments for formatting or grammar or even to not post a comment that I feel is in poor taste or low on merit."
    policy $ do
        toHtml ("By posting a comment, you place it under the Creative Commons " :: String)
        _at "http://creativecommons.org/licenses/by-sa/3.0/us/" "Attribute-Share Alike 3.0"
        toHtml (" license.  (The name and URL you leave as part of the comment submission is the attribution.)" :: String)

formatting_help :: Html ()
formatting_help = p_ [class_ "formatting"] $
    ul_ [class_ "formatting"] $ do
        formatting_ "Paragraphs" " Lines are translated into paragraphs.  Blank lines are ignored."
        formatting_ "Quotes" " Lines prefixed with a | (pipe) are treated as quotations."
        formatting_ "Code" " Lines prefixed with a > are treated as displayed code.  Contiguous blocks of lines prefixed with a > are displayed in the same block."
        formatting "Links" $ do
            toHtml (" Hyperlinks of the form " :: String)
            code_ $ toHtml ("^displayed text|http://example.com^" :: String)
            toHtml (" are supported, but that's it for markup." :: String)

formatting :: String -> Html () -> Html ()
formatting s h = li_ $ do
    strong_ $ toHtml s
    h

formatting_ :: String -> String -> Html ()
formatting_ s = (formatting s) . toHtml

policy :: Html () -> Html ()
policy h = p_ [class_ "policies"] h

policy_ :: String -> Html ()
policy_ = policy . toHtml 