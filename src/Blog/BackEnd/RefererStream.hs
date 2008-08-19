module Blog.BackEnd.RefererStream where

import qualified System.Log.Logger as L
import qualified Blog.FrontEnd.Views as V
import qualified Blog.Constants as C
import qualified Control.Monad as CM
import qualified Data.Map as DM
import Data.List ( isPrefixOf )
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

data RefererStream = RefererStream { control :: Chan Request}

data Request = AddReferer { view_url :: String, referring_url :: String }
             | GetReferers { handback :: MVar Referers }

boot :: IO RefererStream
boot = do { c <- newChan
          ; let rs = RefererStream c
          ; forkIO $ referer_loop rs empty_referers
          ; return rs }

type Referers = DM.Map String (DM.Map String Int)

log_handle :: String
log_handle = "RefererStream"

empty_referers :: Referers
empty_referers = DM.empty

send_referer :: (V.Viewable v) => RefererStream -> v -> String -> IO ()
send_referer rs view e = writeChan ( control rs ) $ AddReferer (V.url view) e

get_referers :: (V.Viewable v) => RefererStream -> v -> IO (DM.Map String Int)
get_referers rs v = do { h <- newEmptyMVar
                       ; writeChan ( control rs) $ GetReferers h
                       ; r <- (takeMVar h)
                       ; return $ DM.findWithDefault DM.empty (V.url v) r }

add_referer :: Request -> Referers -> Referers
add_referer (AddReferer v r) m | C.blog_root `isPrefixOf` r = m
                               | v `DM.member` m = DM.adjust (DM.insertWith' (+) r 1) v m
                               | otherwise = DM.insert v (DM.insert r 1 DM.empty) m

referer_loop :: RefererStream -> Referers -> IO ()
referer_loop rs r = do { req <- readChan . control $ rs
                       ; L.infoM log_handle $ show r
                       ; case req of
                           AddReferer _ _ ->
                               referer_loop rs $ add_referer req r
                           GetReferers h -> 
                               putMVar h r >> referer_loop rs r }


create_referrers_tables_sql :: String
create_referrers_tables_sql =
    "CREATE TABLE referers ( permatitle TEXT PRIMARY KEY NOT NULL, "
    ++ "referring_uri TEXT NOT NULL, "
           ++ "first_hit INTEGER NOT NULL, "
           ++ "most_recent_hit INTEGER NOT NULL, "
           ++ "count INTEGER NOT NULL DEFAULT 0 )"

