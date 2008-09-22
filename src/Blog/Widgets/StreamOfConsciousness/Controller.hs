module Blog.Widgets.StreamOfConsciousness.Controller where

import qualified Blog.Widgets.StreamOfConsciousness.Thought as T
import Blog.BackEnd.HttpPoller

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import qualified System.Log.Logger as L

data Worker = Worker { soc_controller :: SoCController
                     , poller :: HttpPoller } 

data Snapshot = Snapshot { items :: [T.Thought]
                         , max_size :: Int
                         , version :: Int
                         , rendered :: String }
              deriving ( Show, Read, Eq )

data SoCRequest = GetData { snapshot_handback :: MVar Snapshot }
                | GetHtmlFragment { content :: MVar String }
                | Update { retry :: MVar Bool
                         , snapshot :: Snapshot }


data SoCController = SoCController { tid :: ThreadId
                                   , request_channel :: Chan SoCRequest }

start_soc :: Int -> IO SoCController
start_soc mx_sz = do { rc <- newChan
                 ; let snap = Snapshot [] mx_sz 0 ""
                 ; _tid <- forkIO $ loop rc snap
                 ; return $ SoCController _tid rc }

stop_soc :: SoCController -> IO ()
stop_soc = killThread . tid

stop_worker :: Worker -> IO ()
stop_worker = stop_poller . poller

change_worker_polling_frequency :: Worker -> Int -> IO ()
change_worker_polling_frequency w n = change_polling_frequency (poller w) n

get_content :: SoCController -> IO String
get_content c = do { x <- newEmptyMVar 
                   ; writeChan (request_channel c) $ GetHtmlFragment x
                   ; takeMVar x }

get_data :: SoCController -> IO Snapshot
get_data c = do { x <- newEmptyMVar
                ; writeChan (request_channel c) $ GetData x
                ; takeMVar x }

update :: SoCController -> Snapshot -> IO Bool
update c snap = do { ok <- newEmptyMVar 
                   ; writeChan (request_channel c) $ Update ok snap
                   ; takeMVar ok }

collision_delay :: Int
collision_delay = 1000

log_handle :: String
log_handle = "StreamOfConsciousnessController"

commit :: SoCController -> [T.Thought] -> IO ()
commit socc new_items =
    do { snap <- get_data socc
       ; L.infoM log_handle $ "Commit called for " ++ (show $ length new_items) ++ " items."
       ; let items' = take (max_size snap) $ T.merge new_items $ items snap
       ; let rendered' = T.thoughts_to_xhtml items' 
       ; let snap' = snap { items = items'
                          , rendered = rendered' }
       ; ok <- update socc snap'
       ; if ok then
             return ()
         else 
             do { threadDelay collision_delay
                ; L.infoM log_handle $ "Collision detected; waiting."
                ; commit socc new_items }
       }

loop :: Chan SoCRequest -> Snapshot -> IO ()
loop ch snap = 
    do { req <- readChan ch
       ; snap' <- case req of
                   GetHtmlFragment c ->
                       do { putMVar c $ rendered snap
                          ; return snap }
                   GetData h ->
                       do { putMVar h snap
                          ; return snap }
                   Update ok snap'' ->
                       if (version snap) == (version snap'') then
                           do { putMVar ok True
                              ; let snap' = snap'' { version = (version snap) + 1 }
                              ; return snap' }
                       else
                           do { putMVar ok False
                              ; return snap }
       ; loop ch snap' }