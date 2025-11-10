-- | 
module Blog.FrontEnd.Routes ( parse_uri
                            , Route ( XhtmlView, AtomFeed, NoSuchUri, Command, CommentFormView
                                    , CommentSubmission, ReviewComments, PostComment
                                    , DeleteComments, DeleteComment
                                    , AlterComment, ReviewComment
                                    , view, feed, uri, command, permatitle, page_n, int_id )
                            ) where

import Text.ParserCombinators.Parsec
import Data.List (sort)
import qualified Blog.FrontEnd.Views as V
import qualified Blog.FrontEnd.Feeds as F
import qualified Blog.FrontEnd.Actions as A

data Route = XhtmlView { view :: V.View }
           | AtomFeed { feed :: F.Feed }
           | CommentFormView { permatitle :: String }
           | CommentSubmission { permatitle :: String }
           | ReviewComments { page_n :: Maybe Int }
           | ReviewComment { int_id :: Int }
           | PostComment
           | DeleteComment
           | DeleteComments
           | AlterComment { int_id :: Int }
           | Command { command :: A.Action }
           | NoSuchUri { uri :: String }
             deriving (Show, Eq)

parse_uri :: String -> Route
parse_uri u = case (parse uriParser "" u) of
                Left _ -> NoSuchUri $ u
                Right v -> v

uriParser :: Parser Route
uriParser = try pages
            <|> try commands
            <|> try submit_comment
            <|> try post_comment
            <|> try delete_comment
            <|> try delete_comments
            <|> try edit_comment
            <|> do { s <- many anyChar
                   ; return $ NoSuchUri s }

pages :: Parser Route
pages = do { get_method
           ; try single_article
             <|> try articles_by_tags
             <|> try articles      
             <|> try feeds
             <|> try review_pending_comments
             <|> try review_comment
             <|> add_comment_form }

get_method :: Parser String
get_method = do { string "GET " }

post_method :: Parser String
post_method = do { string "POST " }

commands :: Parser Route
commands = do { post_method
              ; (try $ string "/commands") <|> string "/c"
              ; ingest_draft }

submit_comment :: Parser Route
submit_comment = do { post_method
                    ; string "/e/add-comment/"
                    ; t <- p_plink_title
                    ; eof
                    ; return $ CommentSubmission t }

edit_comment :: Parser Route
edit_comment = do { post_method
                  ; string "/x/edit-comment/"
                  ; int_id <- p_int
                  ; eof
                  ; return $ AlterComment int_id }

post_comment :: Parser Route
post_comment = do { post_method
                  ; string "/x/post-comment"
                  ; eof
                  ; return PostComment }

delete_comment :: Parser Route
delete_comment = do { post_method
                    ; string "/x/delete-comment"
                    ; eof
                    ; return DeleteComment }

delete_comments :: Parser Route
delete_comments = do { post_method
                     ; string "/x/delete-comments"
                     ; eof
                     ; return DeleteComments }


review_pending_comments :: Parser Route
review_pending_comments = do { string "/z/review-comments"
                             ; p <- page
                             ; eof
                             ; return $ ReviewComments p }

review_comment :: Parser Route
review_comment = do { string "/z/review-comment/"
                    ; n <- p_int
                    ; eof
                    ; return $ ReviewComment n }

add_comment_form :: Parser Route
add_comment_form = do { string "/c/add-comment/"
                      ; t <- p_plink_title
                      ; eof
                      ; return $ CommentFormView t }

ingest_draft :: Parser Route
ingest_draft = do { string "/post-draft/"
                  ; s <- many1 anyChar
                  ; return $ Command $ A.Ingest s }

feeds :: Parser Route
feeds =  do { (try $ string "/feeds") <|> string "/f"
            ; try articles_feed
              <|> try comments_feeds
              <|> try tag_feed
              <|> try tags_feed
              <|> article_comments_feed }

articles_feed :: Parser Route
articles_feed = do { (try $ string "/articles") <|> string "/a"
                   ; atom_xml
                   ; return $ AtomFeed F.AllPosts }

comments_feeds :: Parser Route
comments_feeds = try all_comments_feed
                 <|> article_comments_feed

all_comments_feed :: Parser Route
all_comments_feed = do { (try $ string "/comments") <|> string "/c"
                       ; atom_xml
                       ; return $ AtomFeed F.AllComments }

article_comments_feed :: Parser Route
article_comments_feed = do { (try $ string "/comments/p/") <|> string "/c/p/"
                           ; t <- p_plink_title
                           ; atom_xml
                           ; return $ AtomFeed $ F.PostComments t }

tag_feed :: Parser Route
tag_feed = do { string "/tag/"
              ; t <- p_tag_name
              ; atom_xml
              ; return $ AtomFeed $ F.ByTag t }

tags_feed :: Parser Route
tags_feed = do { (try $ string "/tags/") <|> string "/t/"
               ; ts <- (p_tag_name `sepBy1` (char ','))
               ; atom_xml
               ; return $ AtomFeed $ F.ByTags (sort ts) }

atom_xml :: Parser ()
atom_xml = do { string "/atom.xml"
              ; eof }

articles :: Parser Route
articles = do { (try $ string "/articles") <|> string "/a"
              ; try all_articles
                <|> try single_article
                <|> try articles_by_date
                <|> try articles_by_tag
                <|> articles_by_tags }

all_articles :: Parser Route
all_articles = do { p <- page
                  ; return $ XhtmlView $ V.All p }

single_article :: Parser Route
single_article = do { string "/p/"
                    ; t <- p_plink_title
                    ; eof
                    ; return $ XhtmlView $ V.ByPermatitle t }

articles_by_tag :: Parser Route
articles_by_tag = do { string "/tag/"
                     ; t <- p_tag_name
                     ; p <- page
                     ; return $ XhtmlView $ V.ByTag t p }

articles_by_tags :: Parser Route
articles_by_tags = do { (try $ string "/t/") <|> string "/tags/"
                      ; ts <- (p_tag_name `sepBy1` (char ','))
                      ; p <- page
                      ; return $ XhtmlView $ V.ByTags (sort ts) p }

articles_by_date :: Parser Route
articles_by_date = do { char '/'
                  ; y <- p_int
                  ; try $ do { p <- page
                             ; return $ XhtmlView $ V.ByYear y p }
                    <|> do { char '/'
                           ; m <- p_int
                           ; try $ do { p <- page
                                      ; return $ XhtmlView $ V.ByMonth y m p }
                             <|> do { char '/'
                                    ; d <- p_int
                                    ; try $ do { p <- page
                                               ; return $ XhtmlView $ V.ByDay y m d p }
                                      <|> do { char '/'
                                             ; title <- p_plink_title
                                             ; eof
                                             ; return $ XhtmlView $ V.ByYMDPermatitle y m d title }
                                    }
                           }
                  }

page :: Parser (Maybe Int)
page = do { try $ do { eof
                     ; return Nothing }
            <|> do { (try $ string "/p/") <|> string "/page/"
                   ; i <- p_int
                   ; eof
                   ; return $ Just i } }

p_plink_title :: Parser String
p_plink_title = many1 (alphaNum <|> char '-' <|> char '_')

p_int :: Parser Int
p_int = do { i <- many1 digit <?> "digits"
           ; return (read i) }

p_tag_name :: Parser String
p_tag_name =  many1 (alphaNum <|> char '-' <|> char '_' <|> char '.' )
