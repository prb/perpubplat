-- perpubplat.hs: servlet to run the blog.

import qualified Blog.Constants as C
import qualified Blog.FrontEnd.Presentation as P
import qualified Blog.FrontEnd.Syndication as S
import qualified Blog.Model.Entry as B
import qualified Blog.Model.CommentForm as CF
import qualified Blog.BackEnd.ModelSupport as MSupp
import qualified Blog.BackEnd.Holder as H
import qualified Blog.BackEnd.RefererStream as RefS
import qualified Blog.BackEnd.HitTracker as HitT
import qualified Blog.Model.CommentQueue as CommentQ
import Blog.FrontEnd.Routes
import qualified Blog.FrontEnd.Views as V
import qualified Blog.FrontEnd.Feeds as F
import qualified Blog.FrontEnd.Actions as A
import qualified Blog.FrontEnd.Urls as U
import qualified Blog.FrontEnd.CommentEntry as CE
import qualified Blog.Admin.PendingComments as PC
import qualified Blog.Widgets.ChromeBackEnd as ChromeB
import qualified Blog.BackEnd.ModelChangeListener as MCL
import qualified Utilities as Utils

import Blog.BackEnd.AsyncLogHandler

import qualified System.Log.Logger as L

import qualified System.IO as SIO

import Data.ByteString.Lazy ( copy )
import Data.ByteString.Lazy.Char8 ( unpack, pack )
import Data.Digest.Pure.MD5 ( md5 )
import Control.Concurrent ( forkIO )
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.List (isPrefixOf)
import Control.Monad (when)
import Network.FastCGI 
import Network.URI ( uriPath )

data Controllers = Controllers { model :: H.Holder B.Model
                               , comment_c :: CommentQ.CommentController
                               , chrome_b :: ChromeB.ChromeBackEnd
                               , referer_stream :: RefS.RefererStream
                               }

serve :: Controllers -> CGI CGIResult
serve con = do { u <- requestURI
               ; p <- progURI
               ; m <- requestMethod
               ; let c = m ++ " " ++ drop (length $ uriPath p) (uriPath u)
               ; let route = parse_uri c
               ; case route of 
                   (NoSuchUri u) -> outputNotFound u
                   (XhtmlView v) -> serve_content con v 
                   (AtomFeed f) -> serve_feed con f
                   (CommentFormView t) -> add_comment_form con t
                   (CommentSubmission t) -> process_comment_form con t
                   (ReviewComments n) -> review_comments con n
                   (ReviewComment n) -> review_comment con n
-- Drafts functionality
--                   (ReviewDrafts n) -> review_drafts con n
--                   (ReviewDraft n) -> review_drafts con n
                   PostComment -> post_comment con
                   DeleteComment -> delete_comment con
                   DeleteComments -> delete_comments con
                   (AlterComment n) -> edit_comment con n
                   (Command c) -> perform_command con c }

serve_content :: (V.Viewable v) => Controllers -> v -> CGI CGIResult
serve_content con v = do { r <- requestHeader "Referer"
                         ; when (isJust r) $ liftIO $ RefS.send_referer (referer_stream con) v (fromJust r)
                         ; setStatus 200 "OK"
                         -- ; setHeader "Content-type" "application/xhtml+xml"
                         ; setHeader "Content-type" "text/html; charset=utf-8"
                         ; m <- liftIO $ H.get (model con)
                         ; when (V.kind v == V.Single) $ 
                               liftIO $ mapM_ (HitT.tally_hit . ChromeB.hit_tracker . chrome_b $ con) $ ((V.lens v) m)
                         ; h <- liftIO $ P.assemble_page v (chrome_b con) m
                         ; output h }

serve_feed :: (F.Feedable f, Show f) => Controllers -> f -> CGI CGIResult
serve_feed con f =
    do { m <- liftIO $ H.get (model con)
       ; let items = F.items f m
       ; let last_updated = S.last_updated items
       ; let etag = show . md5 . pack $ (show f) ++ (last_updated)
       ; ifNoneMatch <- requestHeader "If-None-Match"
       ; let etag_didnt_match = 
                 case ifNoneMatch of
                   Nothing -> True
                   Just e ->
                       e /= etag                                     
       ; ifModifiedSince <- requestHeader "If-Modified-Since"
       {- From the HTTP 1.1 spec:

          "If none of the entity tags match, then the server MAY
          perform the requested method as if the If-None-Match header
          field did not exist, but MUST also ignore any
          If-Modified-Since header field(s) in the request. That is,
          if no entity tags match, then the server MUST NOT return a
          304 (Not Modified) response. "

        -}
       ; let modified = ( ifNoneMatch /= Nothing && etag_didnt_match) 
                        || 
                        ( ( ifNoneMatch == Nothing )
                          && case ifModifiedSince of
                               Nothing -> True
                               Just d -> (Utils.httpDateToIso8601 d) < last_updated )
       ; if modified then
             do { setStatus 200 "OK"
                ; setHeader "Content-type" "application/atom+xml"
                ; setHeader "Last-Modified" $ Utils.iso8601toRfc1123 last_updated
                ; setHeader "ETag" $ etag
                ; output $ S.assemble_feed f m items }
         else
             do { setStatus 304 "Not Modified"
                ; output $ ""}
       }

review_comments :: Controllers -> Maybe Int -> CGI CGIResult
review_comments con n = do { cmts <- liftIO $ CommentQ.fetch_comments (comment_c con)
                           ; m <- liftIO $ H.get (model con)
                           ; let p = case n of
                                       Nothing -> 1
                                       Just i -> i
                           ; output $ PC.display_comments m p cmts }

review_comment :: Controllers -> Int -> CGI CGIResult
review_comment con n = do { cmt <- liftIO $ CommentQ.fetch_comment (comment_c con) n
                          ; case cmt of
                              (Just c) -> 
                                  do { m <- liftIO $ H.get (model con)
                                     ; let i = B.item_by_id m (fromJust $ B.parent c)
                                     ; let cf = CF.from_item c
                                     ; output $ CE.comment_form m i (U.edit_comment_target n) (Just cf) }
                              Nothing ->
                                  redirect $ U.pending_comments Nothing }

edit_comment :: Controllers -> Int -> CGI CGIResult
edit_comment con n = do { cf <- request_to_comment_form
                        ; liftIO $ CommentQ.alter_comment (comment_c con) n cf
                        ; redirect $ U.pending_comments Nothing }

post_comment :: Controllers -> CGI CGIResult
post_comment con = do { int_id <- readInput "id"
                      ; pg <- readInput "page"
                      ; liftIO $ CommentQ.post_comment (model con) (comment_c con) (fromJust int_id)
                      ; redirect $ U.pending_comments pg }

delete_comment :: Controllers -> CGI CGIResult
delete_comment con = do { int_id <- readInput "id"
                        ; pg <- readInput "page"
                        ; case int_id of
                            Nothing ->
                                liftIO $ return ()
                            Just n ->
                                liftIO $ CommentQ.delete_comment (comment_c con) n
                        ; redirect $ U.pending_comments pg }

delete_comments :: Controllers -> CGI CGIResult
delete_comments con = do { fields <- getInputNames
                         ; ids <- (mapM readInput) . (filter (isPrefixOf "id_")) $ fields
                         ; pg <- readInput "page"
                         ; case (catMaybes ids) of
                             [] -> 
                                 liftIO $ return ()
                             just_ids ->
                                 liftIO $ CommentQ.delete_comments (comment_c con) just_ids
                         ; redirect $ U.pending_comments pg }

perform_command :: Controllers -> A.Action -> CGI CGIResult
perform_command con c = case c of
                          (A.Ingest d) ->
                              do { result <- liftIO ( MSupp.ingest_draft (model con) d)
                                 ; case result of
                                     Right i ->
                                         do { m <- liftIO $ H.get (model con)
                                            ; liftIO $ MCL.handle_model_change (chrome_b con) m
                                            ; redirect $ B.permalink m i}
                                     Left err ->
                                         do { setHeader "Content-type" "text/plain"
                                            ; setStatus 400 " Unable to process draft content"
                                            ; output $ err ++ "\n"
                                            }
                                 }

add_comment_form :: Controllers -> String -> CGI CGIResult
add_comment_form con t = do { m <- liftIO $ H.get (model con)
                            ; let i = B.post_by_permatitle m t
                            ; output $ CE.comment_form m i (U.add_comment_target $ B.permatitle i) Nothing }

process_comment_form :: Controllers -> String -> CGI CGIResult
process_comment_form con t = do { cf<- request_to_comment_form
                                ; m <- liftIO $ H.get (model con)
                                ; let i = B.post_by_permatitle m t
                                ; case CF.validate cf of
                                    (_,True) ->
                                        do { liftIO $ CommentQ.add_comment (comment_c con) i cf
                                           ; redirect $ B.permalink m i}
                                    (cf',False) ->
                                        output $ CE.comment_form m i (U.add_comment_target $ B.permatitle i) (Just cf') }

request_to_comment_form :: CGI CF.CommentForm
request_to_comment_form = do { authorName <- getI "authorName"
                             ; authorEmail <- getI "authorEmail"
                             ; authorUri <- getI "authorUri"
                             ; body <- getI "commentBody"
                             ; return $ CF.new_comment_form authorName authorEmail authorUri body }

getI :: (MonadCGI m) => String -> m String
getI s = do { mbs <- getInputFPS s
            ; case mbs of 
                Nothing ->
                    return ""
                (Just bs) ->
                    return $ unpack $! copy bs } -- avoid segfault

main :: IO () 
main = do { root_l <- L.getRootLogger
          ; h <- SIO.openFile C.logfile SIO.AppendMode
          ; logfile_appender <- asyncHandler 10 h L.INFO
          ; L.saveGlobalLogger $ (L.setHandlers [logfile_appender]) . (L.setLevel L.INFO) $ root_l
          ; cq <- CommentQ.boot
          ; mh <- MSupp.boot
          ; cc <- CommentQ.spawn cq
          ; cb <- (H.get mh) >>= ChromeB.boot
          ; rs <- RefS.boot
          ; let con = Controllers mh cc cb rs
          ; runFastCGIConcurrent' forkIO 50 (serve con) }