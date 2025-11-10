module Blog.Model.DraftQueue where

import qualified Blog.Model.CommentBodyParser as CBP
import qualified Blog.Model.Entry as B
import qualified Data.Map as M
import Data.Map ( (!) )
import Data.Maybe
import qualified Blog.Constants as C
import qualified Blog.Model.CommentForm as CF
import Blog.BackEnd.IoOperations ( load )
import qualified Blog.BackEnd.DataController as DC
import Utilities

import System.IO
import qualified System.IO.Error as SIE
import System.FilePath ( (</>) )
import System.Directory ( removeFile, getDirectoryContents, doesFileExist )
import Data.List ( isPrefixOf )
import Control.Monad ( filterM, when )
import Control.Concurrent ( ThreadId, forkIO )
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.Chan ( Chan, newChan,  writeChan, readChan )

import System.Log.Logger
import qualified Data.Time.Clock as DTC

log_handle :: String
log_handle = "DraftManagement"

data Queue = Quque { drafts :: M.Map Int B.Item
                   , next_id :: MVar Int }

data Controller = Controller { request_channel :: Chan Request
                                       , thread_id :: ThreadId }

data Request = Add { form_data :: DF.DraftForm }
             | Edit { draft_id :: Int
                    , form_data :: DF.DraftForm }
             | Delete { draft_id :: Int }
             | Fetch { index :: Int        
                     , handback_item :: MVar (Maybe B.Item) }
             | FetchAll { start :: Int
                     , count :: Int
                     , handback_items :: MVar [B.Item] }
             | FetchAll { handback_queue :: MVar Queue }
             | Boot

spawn :: Queue -> IO Controller
spawn cq = do { c <- newChan
              ; t <- forkIO (loop c cq)
              ; infoM log_handle $ "Forked new Controller on "
                      ++ (show t) ++ "."
              ; return $ Controller c t }

loop :: Chan Request -> Queue -> IO ()
loop c cq = do { req <- readChan c
               ; case req of
                   (Add p cf) ->
                       do { i <- CF.to_item p cf 
                          ; cq' <- add_ cq i
                          ; loop c cq' }
                   (Delete idx) ->
                       do { cq' <- delete_ cq idx
                          ; loop c cq' }
                   (Fetch idx res) ->
                       do { putMVar res $ M.lookup idx (comments cq)
                          ; loop c cq }
                   (FetchAll res) ->
                       do { putMVar res cq
                          ; loop c cq }
                   (Edit k cf) ->
                       do { let cmts = comments cq
                          ; if M.notMember k cmts then
                                loop c cq
                            else
                                do { ts <- now
                                   ; let i' = ((comments cq) ! k) { B.author = B.Author ( CF.value . CF.authorName $ cf )
                                                                               ( Just $ CF.value . CF.authorUri $ cf )
                                                                               ( Just $ CF.value . CF.authorEmail $ cf )
                                                                               False
                                                                  , B.body = CF.value . CF.body $ cf
                                                                  , B.updated = ts }
                                   ; cq' <- alter_ cq i'
                                   ; loop c cq' }
                          }
                   Boot ->
                       do { cq' <- boot
                          ; loop c cq' }
               }

post_comment :: DC.DataController -> Controller -> Int -> IO ()
post_comment dc cc idx = do { c <- fetch cc idx
                            ; case c of
                                (Just i) ->
                                    do { case (CBP.parse_comment $ B.body i) of 
                                           Left _ ->
                                               return ()
                                           Right b -> 
                                               do { let b' =  CBP.blocks_to_string b
                                                  ; DC.post_comment dc (i { B.body = b' }) 
                                                  ; delete_comment cc idx }
                                       }
                                Nothing ->
                                    return ()
                            }

send :: Controller -> Request -> IO ()
send = writeChan . request_channel

fetch :: Controller -> Int -> IO (Maybe B.Item)
fetch cc idx = do { v <- newEmptyMVar
                  ; (send cc) $ FetchComment idx v
                  ; takeMVar v }

alter :: Controller -> Int -> DF.Form -> IO ()
alter cc n f = (send cc) $ Edit n f

fetch_page :: Controller -> Int -> IO [B.Item]
fetch_page c n = do { v <- newEmptyMVar
                    ; (send c) $ FetchAll v
                    ; dq <- takeMVar v
                    ; let ds = map snd $ M.toAscList (drafts dq)
                    ; return $ paginate C.draft_view_size n ds }

fetch_all :: Controller -> IO [B.Item]
fetch_all c = do { v <- newEmptyMVar
                 ; (send cc) $ FetchAll v
                 ; dq <- takeMVar v
                 ; return $ map snd $ M.toAscList (drafts cq) }

add_comment :: Controller -> B.Item -> DF.Form -> IO ()
add_comment c i f = (send c) $ Add i f

delete_comment :: Controller -> Int -> IO ()
delete_comment c idx = (send c) $ Delete idx

empty :: IO Queue
empty = do { n <- newMVar 0
           ; return $ Queue M.empty n }

boot :: IO Queue
boot = do { infoM log_handle $ "Booting Queue from files on disk in " 
                  ++ C.drafts_dir ++ "."
          ; files <- getDirectoryContents C.drafts_dir
          ; let files' = (map (\x -> C.drafts_dir </> x)) . (filter is_filename) $ files
          ; files'' <- filterM (doesFileExist) files'
          ; t1 <- DTC.getCurrentTime
          ; comments <- mapM (load log_handle) files''
          ; let comments' = catMaybes comments
          ; let cnt = length comments'
          ; t2 <- DTC.getCurrentTime
          ; infoM log_handle $ "Loaded " ++ (show cnt) ++ " comments from disk in " ++
                  ( elapsed_hundreths t2 t1 ) ++ " seconds."
          ; case comments' of
              [] ->
                  empty
              _ ->
                  do { let m = map_by (B.internal_id) comments'
                     ; n <- newMVar ((fst . M.findMax $ m) + 1)
                     ; return $ Queue m n } }

alter_ :: Queue -> B.Item -> IO Queue
alter_ q i = do { let q' = q { drafts = M.insert (B.internal_id i) i (drafts q) }
                 ; write_ i
                 ; return q' }
                                

add_ :: Queue -> B.Item -> IO Queue
add_ q i = do { n <- grab_id q
              ; let i' = i { B.internal_id = n }
              ; let q' = q { drafts = M.insert n i' (drafts q) }
              ; write_ i'
              ; return q' }

delete_ :: Queue -> Int -> IO Queue
delete_ q idx = do { let q' = q { drafts = M.delete idx (drafts q) }
                    ; let f = filename idx
                    ; exists <- doesFileExist f
                    ; when exists ((removeFile f)
                                   `SIE.catch`
                                   (handle_error $"Unable to remove data file " ++ f ++ "."))
                    ; return cq' }

handle_error :: String -> SIE.IOError -> IO ()
handle_error s err = errorM log_handle $ s ++ "\nException was: " ++ (show err)

write_ :: B.Item -> IO ()
write_ i = do { let f = filename (B.internal_id i)
              ; exists <- doesFileExist f
              ; when exists $ warningM log_handle $
                         "The filename " ++ f ++ " is already present on disk but "
                         ++ "is not present in the internal data structure; "
                         ++ "the file on disk will be overwritten."
              ; do { h <- openFile f WriteMode
                   ; hPutStr h $ B.to_string i
                   ; hClose h }
                `SIE.catch`
                (handle_error $ "Unable to write comment data file " ++ f
                      ++ " to disk; this may result in data loss if the application is stopped"
                      ++ " before the comment is handled.")
              }

filename :: Int -> FilePath
filename k = C.comment_dir </> ("pending-draft-" ++ (show k))

is_filename :: FilePath -> Bool
is_filename = isPrefixOf "pending-draft-"

next_valid_id :: Int -> IO Int
next_valid_id n = do { exists <- doesFileExist $ filename n
                     ; if exists then
                           next_valid_id (n+1)
                       else
                           return n }

grab_id :: Queue -> IO Int
grab_id q = do { n <- takeMVar $ next_id q
               ; n' <- next_valid_id n
               ; debugM log_handle $ "Allocated ID " ++ (show n') ++ "."
               ; putMVar (next_id q) (n'+1)
               ; return n' }