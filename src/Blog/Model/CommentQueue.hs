module Blog.Model.CommentQueue where

import qualified Blog.Model.CommentBodyParser as CBP
import qualified Blog.Model.Entry as B
import qualified Data.Map as M
import Data.Map ( (!) )
import Data.Maybe
import qualified Blog.Constants as C
import qualified Blog.Model.CommentForm as CF
import Blog.BackEnd.IoOperations ( load, log_load_errors_ )
import qualified Blog.BackEnd.Holder as H
import qualified Blog.BackEnd.ModelSupport as MSupp
import Utilities

import System.IO
import qualified System.IO.Error as SIE
import System.FilePath ( (</>) )
import System.Directory ( removeFile, getDirectoryContents, doesFileExist )
import Data.List ( isPrefixOf )
import Control.Monad ( filterM, when, foldM )
import Control.Concurrent ( ThreadId, forkIO )
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.Chan ( Chan, newChan,  writeChan, readChan )

import System.Log.Logger
import System.Time

log_handle :: String
log_handle = "CommentQueue"

data CommentQueue = CommentQueue { comments :: M.Map Int B.Item
                                 , next_id :: MVar Int }

data CommentController = CommentController { request_channel :: Chan Request
                                           , thread_id :: ThreadId }

data Request = AddComment { parent :: B.Item
                          , form_data :: CF.CommentForm }
             | EditComment { comment_id :: Int
                           , form_data :: CF.CommentForm }
             | DeleteComment { comment_id :: Int }
             | DeleteComments { comment_ids :: [Int] }
             | FetchComment { index :: Int                  
                            , handback_item :: MVar (Maybe B.Item) }
             | FetchComments { start :: Int
                             , count :: Int
                             , handback_items :: MVar [B.Item] }
             | FetchAllComments { handback_queue :: MVar CommentQueue }
             | Boot

spawn :: CommentQueue -> IO CommentController
spawn cq = do { c <- newChan
              ; t <- forkIO (loop c cq)
              ; infoM log_handle $ "Forked new CommentController on thread "
                      ++ (show t) ++ "."
              ; return $ CommentController c t }

loop :: Chan Request -> CommentQueue -> IO ()
loop c cq = do { req <- readChan c
               ; case req of
                   (AddComment p cf) ->
                       do { i <- CF.to_item p cf 
                          ; cq' <- add_ cq i
                          ; loop c cq' }
                   (DeleteComment idx) ->
                       do { cq' <- delete_ cq idx
                          ; loop c cq' }
                   (DeleteComments idxs) ->
                       do { cq' <- foldM delete_ cq idxs
                          ; loop c cq' }
                   (FetchComment idx res) ->
                       do { putMVar res $ M.lookup idx (comments cq)
                          ; loop c cq }
                   (FetchAllComments res) ->
                       do { putMVar res cq
                          ; loop c cq }
                   (EditComment k cf) ->
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

post_comment :: (H.Holder B.Model) -> CommentController -> Int -> IO ()
post_comment h cc idx = do { c <- fetch_comment cc idx
                           ; case c of
                               (Just i) ->
                                   do { case (CBP.parse_comment $ B.body i) of 
                                          Left _ ->
                                              return ()
                                          Right b -> 
                                              do { let b' =  CBP.blocks_to_string b
                                                 ; MSupp.post_comment h (i { B.body = b' }) 
                                                 ; delete_comment cc idx }
                                       }
                               Nothing ->
                                    return ()
                           }

send :: CommentController -> Request -> IO ()
send = writeChan . request_channel

fetch_comment :: CommentController -> Int -> IO (Maybe B.Item)
fetch_comment cc idx = do { v <- newEmptyMVar
                        ; (send cc) $ FetchComment idx v
                        ; takeMVar v }

alter_comment :: CommentController -> Int -> CF.CommentForm -> IO ()
alter_comment cc n cf = (send cc) $ EditComment n cf

fetch_comments_page :: CommentController -> Int -> IO [B.Item]
fetch_comments_page cc n = do { v <- newEmptyMVar
                              ; (send cc) $ FetchAllComments v
                              ; cq <- takeMVar v
                              ; let cmts = map snd $ M.toAscList (comments cq)
                              ; return $ paginate C.comment_view_size n cmts }

fetch_comments :: CommentController -> IO [B.Item]
fetch_comments cc = do { v <- newEmptyMVar
                       ; (send cc) $ FetchAllComments v
                       ; cq <- takeMVar v
                       ; return $ map snd $ M.toAscList (comments cq) }

add_comment :: CommentController -> B.Item -> CF.CommentForm -> IO ()
add_comment cc i cf = (send cc) $ AddComment i cf

delete_comment :: CommentController -> Int -> IO ()
delete_comment cc idx = (send cc) $ DeleteComment idx

delete_comments :: CommentController -> [Int] -> IO ()
delete_comments cc idxs = (send cc) $ DeleteComments idxs

empty :: IO CommentQueue
empty = do { n <- newMVar 0
           ; return $ CommentQueue M.empty n }

boot :: IO CommentQueue
boot = do { infoM log_handle $ "Booting CommentQueue from files on disk in " 
                  ++ C.comment_dir ++ "."
          ; files <- getDirectoryContents C.comment_dir
          ; let files' = (map (\x -> C.comment_dir </> x)) . (filter is_filename) $ files
          ; files'' <- filterM (doesFileExist) files'
          ; t1 <- getClockTime
          ; comments <- mapM (load log_handle) files''
          ; let comments' = rights comments
          ; let errors = lefts comments
          ; when (length errors > 0) $ errorM log_handle $ "Failed to load " ++ (show $ length errors) 
                 ++ " comments form disk."
          ; log_load_errors_ log_handle errors 
          ; let cnt = length comments'
          ; t2 <- getClockTime
          ; infoM log_handle $ "Loaded " ++ (show cnt) ++ " comments from disk in " ++
                  ( elapsed_hundreths t2 t1 ) ++ " seconds."
          ; case comments' of
              [] ->
                  empty
              _ ->
                  do { let m = map_by (B.internal_id) comments'
                     ; n <- newMVar ((fst . M.findMax $ m) + 1)
                     ; return $ CommentQueue m n } }

alter_ :: CommentQueue -> B.Item -> IO CommentQueue
alter_ cq i = do { let cq' = cq { comments = M.insert (B.internal_id i) i (comments cq) }
                 ; write_ i
                 ; return cq' }
                                

add_ :: CommentQueue -> B.Item -> IO CommentQueue
add_ cq i = do { n <- grab_id cq
               ; let i' = i { B.internal_id = n }
               ; let cq' = cq { comments = M.insert n i' (comments cq) }
               ; write_ i'
               ; return cq' }

delete_ :: CommentQueue -> Int -> IO CommentQueue
delete_ cq idx = do { let cq' = cq { comments = M.delete idx (comments cq) }
                    ; let f = filename idx
                    ; exists <- doesFileExist f
                    ; when exists ((removeFile f)
                                   `SIE.catch`
                                   (handle_error $"Unable to remove comment data file " ++ f ++ "."))
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
filename k = C.comment_dir </> ("pending-comment-" ++ (show k))

is_filename :: FilePath -> Bool
is_filename = isPrefixOf "pending-comment-"

next_valid_id :: Int -> IO Int
next_valid_id n = do { exists <- doesFileExist $ filename n
                     ; if exists then
                           next_valid_id (n+1)
                       else
                           return n }

grab_id :: CommentQueue -> IO Int
grab_id cq = do { n <- takeMVar $ next_id cq
                ; n' <- next_valid_id n
                ; debugM log_handle $ "Allocated ID " ++ (show n') ++ "."
                ; putMVar (next_id cq) (n'+1)
                ; return n' }