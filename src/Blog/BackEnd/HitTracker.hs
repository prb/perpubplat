module Blog.BackEnd.HitTracker where

import qualified Database.HDBC as DH
import qualified Database.HDBC.Sqlite3 as DHS3
import qualified Blog.Constants as C
import qualified Control.Monad as CM
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Chan as CCC
import qualified Control.Concurrent.MVar as CCM
import Blog.Model.Entry ( Item ( permatitle ) )
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified System.Log.Logger as L

data HitTracker = HitTracker { events :: CCC.Chan Request
                             , database_worker :: DatabaseWorker
                             , hits :: !(DM.Map String Int)
                             , tracker_tid :: CC.ThreadId }

data Request = TallyHit { h_item :: Item }
             | GetAllHits { hb :: CCM.MVar (DM.Map String Int) }
             | GetHits { h_item :: Item
                       , gh_hb :: CCM.MVar Int }             

data DatabaseWorker = DatabaseWorker { queue :: CCC.Chan DatabaseRequest
                                     , db_worker_tid :: CC.ThreadId }

data ConnectionState = ConnectionState { connection :: DHS3.Connection
                                       , initialize_stmt :: DH.Statement
                                       , update_stmt :: DH.Statement }

data DatabaseRequest = Initialize { db_item :: Item }
                     | SetCount { db_item :: Item 
                                 , count :: Int }

log_handle :: String
log_handle = "HitTracker"

boot :: IO HitTracker
boot = do { defaultTid <- CC.myThreadId
          ; conn <- DHS3.connectSqlite3 C.sqlite_hits_db_filename
          ; checkVersion "3.4.0" conn
          ; L.infoM log_handle $ "Connected to SQLite3 database " ++ C.sqlite_hits_db_filename ++ " in boot on "
                        ++ (show defaultTid) ++ "."
          ; tables <- DH.getTables conn 
          ; CM.unless ("hit_counts" `DL.elem` tables) $ 
            ( DH.run conn table_create_sql [] >> DH.commit conn ) >> return ()
          ; cnt <- count_rows conn "hit_counts"
          ; L.infoM log_handle $ "Found " ++ (show cnt) ++ " rows in the database; processing."
          ; vals <- DH.quickQuery' conn select_all_sql []
          ; L.infoM log_handle $ "Processed " ++ (show $ length vals) ++ " rows from the database; building internal structures."
          ; DH.disconnect conn
          ; let hit_map = foldl to_pair DM.empty vals
          ; q_c <- CCC.newChan
          ; let db_w = DatabaseWorker q_c defaultTid
          ; db_tid <- CC.forkIO $ db_loop db_w Nothing
          ; r_c <- CCC.newChan
          ; let h = HitTracker r_c ( db_w  { db_worker_tid = db_tid } ) hit_map defaultTid
          ; h_tid <- CC.forkIO $ loop h
          ; return $ h { tracker_tid = h_tid} }

checkVersion :: DH.IConnection conn => String -> conn -> IO ()
checkVersion min_ver c = L.infoM log_handle $ "Client version is " ++ (DH.proxiedClientVer c)
                         ++ "; this may cause problems if it is less than " ++ min_ver ++ "."

to_pair :: DM.Map String Int -> [DH.SqlValue] -> DM.Map String Int
to_pair m [k,v] = DM.insert (DH.fromSql k) (DH.fromSql v) m 

count_rows :: DH.IConnection conn => conn -> String -> IO Int
count_rows c t = do { [[ cnt ]] <- DH.quickQuery' c ("SELECT count(*) FROM " ++ t) []
                    ; return . read . DH.fromSql $ cnt }

table_create_sql :: String 
table_create_sql = "CREATE TABLE hit_counts (ptitle TEXT NOT NULL, hits INTEGER NOT NULL DEFAULT 0)"

select_count_sql :: String
select_count_sql = "SELECT count(*) FROM hit_counts"

select_all_sql :: String
select_all_sql = "SELECT * FROM hit_counts"

update_count_sql :: String
update_count_sql = "UPDATE hit_counts SET hits=? WHERE ptitle=?" 

initialize_count_sql :: String
initialize_count_sql = "INSERT INTO hit_counts (ptitle,hits) VALUES (?,1)"

get_hits :: HitTracker -> Item -> IO Int
get_hits h i = do { hb <- CCM.newEmptyMVar
                  ; CCC.writeChan ( events h) $ GetHits i hb
                  ; CCM.takeMVar hb }

tally_hit :: HitTracker -> Item -> IO ()
tally_hit h i = CCC.writeChan (events h) $ TallyHit i

get_all_hits :: HitTracker -> IO (DM.Map String Int)
get_all_hits h = do { hb <- CCM.newEmptyMVar
                    ; CCC.writeChan (events h) $ GetAllHits hb
                    ; CCM.takeMVar hb }

send_hit_to_db :: HitTracker -> Item -> Int -> IO ()
send_hit_to_db h i 1 = CCC.writeChan ( queue $ database_worker h ) $ Initialize i
send_hit_to_db h i c = CCC.writeChan ( queue $ database_worker h ) $ SetCount i c

loop :: HitTracker -> IO ()
loop h = do { req <- CCC.readChan $ events h
            ; case req of
                TallyHit item ->
                    do { let new_hits = DM.insertWith (+) (permatitle item) 1 ( hits h )
                       ; send_hit_to_db h item (new_hits DM.! (permatitle item))
                       ; loop h { hits = new_hits }
                       }
                GetHits item hb ->
                    do { let cnt = DM.findWithDefault 0 (permatitle item) (hits h)
                       ; CCM.putMVar hb cnt
                       ; loop h
                       }
                GetAllHits hb -> 
                    do { CCM.putMVar hb (hits h)
                       ; loop h
                       }
            }

db_loop :: DatabaseWorker -> Maybe ConnectionState -> IO ()
db_loop dw Nothing =
    do { myTid <- CC.myThreadId
       ; conn <- DHS3.connectSqlite3 C.sqlite_hits_db_filename
       ; L.infoM log_handle $ "Connected to SQLite3 database " ++ C.sqlite_hits_db_filename ++ " in db_loop on "
                     ++ (show myTid) ++ "." 
       ; u_st <- DH.prepare conn update_count_sql
       ; i_st <- DH.prepare conn initialize_count_sql
       ; db_loop dw $ Just $ ConnectionState conn i_st u_st }
db_loop dw c@(Just cs) =
    do { req <- CCC.readChan $ queue dw
       ; do { case req of 
                SetCount item val ->
                    do { L.debugM log_handle $ "Post permatitle " ++ (permatitle item)
                         ++ " is already tracked; updating database with new count of "
                         ++ (show val) ++ "."
                       ; r <- DH.execute (update_stmt cs) [ DH.iToSql val, DH.toSql (permatitle item) ]
                       ; CM.unless (r == 1) $ L.warningM log_handle $ "Whoa."
                       }
                Initialize item -> 
                    do { L.debugM log_handle $ "Post id " ++ (permatitle item) ++ " is not yet tallied; inserting new record into database."
                       ; r <- DH.execute (initialize_stmt cs) [ DH.toSql (permatitle item) ]
                       ; CM.unless (r == 1) $ L.warningM log_handle $ "Whoa!"
                       }
            ; L.debugM log_handle $ "Committing."
            ; DH.commit (connection cs) } 
                       `DH.catchSql` (log_sql_error "db_loop")
       ; db_loop dw c
       }

log_sql_error :: String -> DH.SqlError -> IO ()
log_sql_error f se = L.errorM log_handle $ "Encountered SqlError in " ++ f ++ ": state=" ++ (DH.seState se)
                     ++ ", native_error=" ++ (show $ DH.seNativeError se) ++ ", message="
                     ++ (DH.seErrorMsg se) 