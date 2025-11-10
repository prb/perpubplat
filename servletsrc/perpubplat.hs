-- perpubplat.hs: web server for the blog using Warp/WAI

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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Digest.Pure.MD5 ( md5 )
import Data.Maybe (fromJust, isJust, catMaybes, fromMaybe)
import Data.List (isPrefixOf, lookup)
import qualified Data.CaseInsensitive as CI
import Control.Monad (when)

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types
import Network.HTTP.Types.Header (hETag)
import Network.URI ( uriPath )

data Controllers = Controllers { model :: H.Holder B.Model
                               , comment_c :: CommentQ.CommentController
                               , chrome_b :: ChromeB.ChromeBackEnd
                               , referer_stream :: RefS.RefererStream
                               }

-- Main WAI application
serve :: Controllers -> Application
serve con req respond = do
    let method = BS8.unpack $ requestMethod req
        path = BS8.unpack $ rawPathInfo req
        routeStr = method ++ " " ++ path
        route = parse_uri routeStr

    case route of
        (NoSuchUri u) -> respond $ notFound u
        (XhtmlView v) -> serve_content con req respond v
        (AtomFeed f) -> serve_feed con req respond f
        (CommentFormView t) -> add_comment_form con req respond t
        (CommentSubmission t) -> process_comment_form con req respond t
        (ReviewComments n) -> review_comments con req respond n
        (ReviewComment n) -> review_comment con req respond n
        PostComment -> post_comment con req respond
        DeleteComment -> delete_comment con req respond
        DeleteComments -> delete_comments con req respond
        (AlterComment n) -> edit_comment con req respond n
        (Command c) -> perform_command con req respond c

-- Helper to get header value
getHeader :: HeaderName -> Request -> Maybe String
getHeader name req = fmap BS8.unpack $ Prelude.lookup name (requestHeaders req)

-- Helper to parse query/form parameters
getParam :: BS.ByteString -> Request -> IO (Maybe String)
getParam name req = do
    let qparams = queryString req
    return $ case Prelude.lookup name qparams of
        Just (Just v) -> Just (BS8.unpack v)
        _ -> Nothing

serve_content :: (V.Viewable v) => Controllers -> Request -> (Response -> IO ResponseReceived) -> v -> IO ResponseReceived
serve_content con req respond v = do
    let referer = getHeader (CI.mk $ BS8.pack "Referer") req
    when (isJust referer) $ RefS.send_referer (referer_stream con) v (fromJust referer)

    m <- H.get (model con)
    when (V.kind v == V.Single) $
        mapM_ (HitT.tally_hit . ChromeB.hit_tracker . chrome_b $ con) $ ((V.lens v) m)

    html <- P.assemble_page v (chrome_b con) m
    respond $ responseLBS status200
        [(hContentType, BS8.pack "text/html; charset=utf-8")]
        (LBS8.pack html)

serve_feed :: (F.Feedable f, Show f) => Controllers -> Request -> (Response -> IO ResponseReceived) -> f -> IO ResponseReceived
serve_feed con req respond f = do
    m <- H.get (model con)
    let items = F.items f m
        last_updated = S.last_updated items
        etag = show . md5 . LBS8.pack $ (show f) ++ last_updated
        ifNoneMatch = getHeader (CI.mk $ BS8.pack "If-None-Match") req
        etag_didnt_match = case ifNoneMatch of
            Nothing -> True
            Just e -> e /= etag
        ifModifiedSince = getHeader (CI.mk $ BS8.pack "If-Modified-Since") req
        modified = (ifNoneMatch /= Nothing && etag_didnt_match)
                   || (ifNoneMatch == Nothing && case ifModifiedSince of
                        Nothing -> True
                        Just d -> (Utils.httpDateToIso8601 d) < last_updated)

    if modified
        then respond $ responseLBS status200
            [ (hContentType, BS8.pack "application/atom+xml")
            , (CI.mk $ BS8.pack "Last-Modified", BS8.pack $ Utils.iso8601toRfc1123 last_updated)
            , (hETag, BS8.pack etag)
            ]
            (LBS8.pack $ S.assemble_feed f m items)
        else respond $ responseLBS status304 [] LBS.empty

review_comments :: Controllers -> Request -> (Response -> IO ResponseReceived) -> Maybe Int -> IO ResponseReceived
review_comments con req respond n = do
    cmts <- CommentQ.fetch_comments (comment_c con)
    m <- H.get (model con)
    let p = fromMaybe 1 n
        html = PC.display_comments m p cmts
    respond $ responseLBS status200 [(hContentType, BS8.pack "text/html; charset=utf-8")] (LBS8.pack html)

review_comment :: Controllers -> Request -> (Response -> IO ResponseReceived) -> Int -> IO ResponseReceived
review_comment con req respond n = do
    cmt <- CommentQ.fetch_comment (comment_c con) n
    case cmt of
        Just c -> do
            m <- H.get (model con)
            let i = B.item_by_id m (fromJust $ B.parent c)
                cf = CF.from_item c
                html = CE.comment_form m i (U.edit_comment_target n) (Just cf)
            respond $ responseLBS status200 [(hContentType, BS8.pack "text/html; charset=utf-8")] (LBS8.pack html)
        Nothing ->
            respond $ redirectTo $ U.pending_comments Nothing

edit_comment :: Controllers -> Request -> (Response -> IO ResponseReceived) -> Int -> IO ResponseReceived
edit_comment con req respond n = do
    cf <- request_to_comment_form req
    CommentQ.alter_comment (comment_c con) n cf
    respond $ redirectTo $ U.pending_comments Nothing

post_comment :: Controllers -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
post_comment con req respond = do
    int_id <- getParam (BS8.pack "id") req
    pg <- getParam (BS8.pack "page") req
    CommentQ.post_comment (model con) (comment_c con) (read $ fromJust int_id)
    respond $ redirectTo $ U.pending_comments (fmap read pg)

delete_comment :: Controllers -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
delete_comment con req respond = do
    int_id <- getParam (BS8.pack "id") req
    pg <- getParam (BS8.pack "page") req
    case int_id of
        Nothing -> return ()
        Just n -> CommentQ.delete_comment (comment_c con) (read n)
    respond $ redirectTo $ U.pending_comments (fmap read pg)

delete_comments :: Controllers -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
delete_comments con req respond = do
    let qparams = queryString req
        idFields = filter (isPrefixOf "id_" . BS8.unpack . fst) qparams
        ids = catMaybes $ map (\(_,v) -> fmap (read . BS8.unpack) v) idFields
    pg <- getParam (BS8.pack "page") req
    case ids of
        [] -> return ()
        just_ids -> CommentQ.delete_comments (comment_c con) just_ids
    respond $ redirectTo $ U.pending_comments (fmap read pg)

perform_command :: Controllers -> Request -> (Response -> IO ResponseReceived) -> A.Action -> IO ResponseReceived
perform_command con req respond c = case c of
    (A.Ingest d) -> do
        result <- MSupp.ingest_draft (model con) d
        case result of
            Right i -> do
                m <- H.get (model con)
                MCL.handle_model_change (chrome_b con) m
                respond $ redirectTo $ B.permalink m i
            Left err ->
                respond $ responseLBS status400
                    [(hContentType, BS8.pack "text/plain")]
                    (LBS8.pack $ err ++ "\n")

add_comment_form :: Controllers -> Request -> (Response -> IO ResponseReceived) -> String -> IO ResponseReceived
add_comment_form con req respond t = do
    m <- H.get (model con)
    let i = B.post_by_permatitle m t
        html = CE.comment_form m i (U.add_comment_target $ B.permatitle i) Nothing
    respond $ responseLBS status200 [(hContentType, BS8.pack "text/html; charset=utf-8")] (LBS8.pack html)

process_comment_form :: Controllers -> Request -> (Response -> IO ResponseReceived) -> String -> IO ResponseReceived
process_comment_form con req respond t = do
    cf <- request_to_comment_form req
    m <- H.get (model con)
    let i = B.post_by_permatitle m t
    case CF.validate cf of
        (_, True) -> do
            CommentQ.add_comment (comment_c con) i cf
            respond $ redirectTo $ B.permalink m i
        (cf', False) -> do
            let html = CE.comment_form m i (U.add_comment_target $ B.permatitle i) (Just cf')
            respond $ responseLBS status200 [(hContentType, BS8.pack "text/html; charset=utf-8")] (LBS8.pack html)

request_to_comment_form :: Request -> IO CF.CommentForm
request_to_comment_form req = do
    authorName <- getParam (BS8.pack "authorName") req
    authorEmail <- getParam (BS8.pack "authorEmail") req
    authorUri <- getParam (BS8.pack "authorUri") req
    body <- getParam (BS8.pack "commentBody") req
    return $ CF.new_comment_form
        (fromMaybe "" authorName)
        (fromMaybe "" authorEmail)
        (fromMaybe "" authorUri)
        (fromMaybe "" body)

-- Helper functions
notFound :: String -> Response
notFound url = responseLBS status404
    [(hContentType, BS8.pack "text/html")]
    (LBS8.pack $ "<h1>404 Not Found</h1><p>" ++ url ++ "</p>")

redirectTo :: String -> Response
redirectTo url = responseLBS status302
    [(hLocation, BS8.pack url)]
    LBS.empty

main :: IO ()
main = do
    root_l <- L.getRootLogger
    h <- SIO.openFile C.logfile SIO.AppendMode
    logfile_appender <- asyncHandler 10 h L.INFO
    L.saveGlobalLogger $ (L.setHandlers [logfile_appender]) . (L.setLevel L.INFO) $ root_l

    cq <- CommentQ.boot
    mh <- MSupp.boot
    cc <- CommentQ.spawn cq
    cb <- (H.get mh) >>= ChromeB.boot
    rs <- RefS.boot

    let con = Controllers mh cc cb rs
        port = 3000

    L.infoM "perpubplat" $ "Starting server on port " ++ show port
    putStrLn $ "Perpubplat server running on http://localhost:" ++ show port
    run port (serve con)
