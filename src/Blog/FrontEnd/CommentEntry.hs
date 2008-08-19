module Blog.FrontEnd.CommentEntry where

import qualified Blog.Model.Entry as B
import qualified Blog.Model.CommentForm as CF
import Blog.FrontEnd.ContentAtoms

import Text.XHtml.Strict

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
    showHtml . concatHtml $
                 [ topmatter ( "Comment on " ++ B.permatitle i )
                 , body . ( divid "container" )
                            . concatHtml $ [ heading                                     
                                           , ( divid "comment_entry" )
                                             . concatHtml $ [ h3
                                                              . concatHtml $ [ stringToHtml "Comment on "
                                                                             , _a plink (primHtml $ B.title i) ]
                                                            , build_form targ plink cf ]
                                           , comment_sidebar
                                           , footer ] ]
    where
      plink = B.permalink m i


build_form :: String -> String -> Maybe CF.CommentForm -> Html
build_form targ plink cf = ( form ! [ action targ
                                    , method "post" ] )
                           . concatHtml $ [ textarea ! [ name "commentBody"
                                                       , theclass "body_entry"
                                                       , rows "10" ]
                                            << (stringToHtml . CF.value $ bod)
                                          , case bod of
                                              (CF.Field _ _ (Just m)) ->
                                                 ( p ! [ theclass "comment_datum" ]) . 
                                                     ( thespan ! [ theclass "required" ]) . stringToHtml $ m
                                              _ ->
                                                  noHtml
                                          , inp "Name" True "authorName" nam                                          
                                          , inp "Email (will not be published)" True "authorEmail" email
                                          , inp "URL" True "authorUri" uri
                                          , input ! [ theclass "comment_submit"
                                                    , thetype "submit"
                                                    , value "Post Comment" ]
                                          , stringToHtml " or just go "
                                          , _at plink "back"
                                          , stringToHtml "." ]
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

topmatter :: String -> Html
topmatter tit = header . concatHtml $ [ thetitle . stringToHtml $ tit
                                      , base_url
                                      , stylesheet
                                      , robots ]

inp :: String -> Bool -> String -> CF.Field -> Html
inp f r n v = ( p ! [ theclass "comment_datum" ] )
              . concatHtml $ [ input ! [ name n
                                       , theclass "datum_entry"
                                       , value . CF.value $ v ]
                             , if r then
                                   (thespan ! [ theclass "required" ]) . stringToHtml $ "*"
                               else 
                                   noHtml
                             , stringToHtml (" " ++ f)
                             , case v of
                                 (CF.Field _ _ Nothing) ->
                                     noHtml
                                 (CF.Field _ _ (Just m)) -> 
                                     br +++ ( ( thespan ! [ theclass "required" ]) . stringToHtml $ m )
                             ]

robots :: Html
robots = meta ! [ name "ROBOTS", content "NOINDEX, FOLLOW"]

comment_sidebar :: Html
comment_sidebar = ( divid "sidebar" )
                  . concatHtml $ [ h3 . stringToHtml $ "Comment Formatting"
                                 , formatting_help
                                 , h3 . stringToHtml $ "Comment Policies"
                                 , policy_ "All comments are moderated to avoid spam, so your comment won't appear until after I've had a chance to look at it.  Once approved, comments are displayed in the order received."
                                 , policy_ "You can find out when your comment appears and track other comments on this entry by subscribing to the entry's comment feed."
                                 , policy_ "I'll post pretty much anything that's not purely inflammatory, but I do reserve the right to edit comments for formatting or grammar or even to not post a comment that I feel is in poor taste or low on merit."
                                 , policy . concatHtml $ [ stringToHtml "By posting a comment, you place it under the Creative Commons "
                                                         , _at "http://creativecommons.org/licenses/by-sa/3.0/us/" "Attribute-Share Alike 3.0"
                                 , stringToHtml " license.  (The name and URL you leave as part of the comment submission is the attribution.)" ] ]

formatting_help :: Html
formatting_help = ( p ! [ theclass "formatting" ] )
                  . ( ulist ! [ theclass "formatting" ] )
                  . concatHtml $ [ formatting_ "Paragraphs"
                                               " Lines are translated into paragraphs.  Blank lines are ignored."
                                 , formatting_ "Quotes"
                                               " Lines prefixed with a | (pipe) are treated as quotations."
                                 , formatting_ "Code"
                                               " Lines prefixed with a > are treated as displayed code.  Contiguous blocks of lines prefixed with a > are displayed in the same block."
                                 , formatting "Links"
                                              $ concatHtml [ stringToHtml " Hyperlinks of the form "
                                                           , thecode
                                                             << stringToHtml
                                                                "^displayed text|http://example.com^"
                                                           , stringToHtml
                                                             " are supported, but that's it for markup." ] ]

formatting :: String -> Html -> Html
formatting s h = li . concatHtml $ [ bold . stringToHtml $ s
                                   , h ]

formatting_ :: String -> String -> Html
formatting_ s = (formatting s) . stringToHtml

policy :: Html -> Html
policy h = p ! [ theclass "policies" ] << h

policy_ :: String -> Html
policy_ = policy . stringToHtml 