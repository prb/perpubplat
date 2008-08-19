module Blog.BackEnd.Holder where

import Control.Concurrent.MVar ( MVar, newEmptyMVar, takeMVar, putMVar )
import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan )
import Control.Concurrent ( forkIO, ThreadId, killThread )

data Holder a = Holder { requests :: Chan (Request a)
                       , tid :: ThreadId }

data Request a = Get { handback :: MVar a }
               | Put { content :: a }
               | Apply { handback :: MVar a
                       , mutation :: (a -> a) }
               | ApplyIO { handback :: MVar a
                         , mutationIO :: (a -> IO a) }
               | Apply' { mutation :: (a -> a) }
               | ApplyIO' { mutationIO :: (a -> IO a) }

newHolder :: a -> IO (Holder a)
newHolder v = do { c <- newChan
                 ; t <- forkIO $ loop c v
                 ; return $ Holder c t }

get :: Holder a -> IO a
get h = do { hb <- newEmptyMVar
           ; writeChan (requests h) $ Get hb
           ; takeMVar hb }

put :: Holder a -> a -> IO ()
put h s = writeChan (requests h) $ Put s

apply :: Holder a -> (a -> a) -> IO a
apply h f = do { hb <- newEmptyMVar
               ; writeChan (requests h) $ Apply hb f
               ; takeMVar hb }

applyIO :: Holder a -> (a -> IO a) -> IO a
applyIO h f = do { hb <- newEmptyMVar
                 ; writeChan (requests h) $ ApplyIO hb f 
                 ; takeMVar hb }

apply' :: Holder a -> (a -> a) -> IO ()
apply' h f = writeChan (requests h) $ Apply' f

applyIO' :: Holder a -> (a -> IO a) -> IO ()
applyIO' h f = writeChan (requests h) $ ApplyIO' f

kill :: Holder a -> IO ()
kill = killThread . tid

loop :: Chan (Request a) -> a -> IO ()
loop c v = do { r <- readChan c
              ; case r of 
                  Get hb ->
                      putMVar hb v >> loop c v
                  Put v ->
                      loop c v
                  Apply hb f ->
                      do { let v' = f $! v
                         ; putMVar hb v' >> loop c v' }
                  ApplyIO hb f ->
                      do { v' <- f $! v
                         ; putMVar hb v' >> loop c v' }
                  Apply' f ->
                      loop c (f $! v)
                  ApplyIO' f ->
                      (f $! v) >>= (loop c)
              }
                    
