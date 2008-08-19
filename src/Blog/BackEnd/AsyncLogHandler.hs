module Blog.BackEnd.AsyncLogHandler (AsyncLogHandler, asyncHandler) where

import System.Log.Handler
import System.IO
import System.Log

import Utilities ( now )

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan )
import qualified Control.Exception as CE
type Timestamp = String

type LogMessage = String

data AsyncLogHandler = AsyncLogHandler { channel :: Chan (LogRecord, LogMessage, Timestamp)
                                       , level :: Priority }

instance LogHandler AsyncLogHandler where
    setLevel alh p = alh { level = p }
    getLevel alh = level alh
    emit alh lr msg = do { n <- now
                         ; writeChan (channel alh) (lr,msg,n) }
    close _ = return () -- make this better

asyncHandler :: Int -> Handle -> Priority -> IO AsyncLogHandler
asyncHandler n h pri = do { c <- newChan
                          ; forkIO $ append n 0 h c
                          ; return $ AsyncLogHandler { channel = c
                                                     , level = pri } }


append :: Int -> Int -> Handle -> Chan (LogRecord, LogMessage, Timestamp) -> IO ()
append n i h c = do { ((p,m),l,ts) <- readChan c
                    ; (hPutStrLn h $ ts ++ " [" ++ (show p) ++ "] " ++ l ++ " - " ++ m)
                      `CE.catch` (printex h)
                    ; if i == n then
                          do { (hFlush h) `CE.catch` (printex h)
                             ; append n 0 h c }
                      else
                          append n (i+1) h c }

printex :: Handle -> CE.Exception -> IO ()
printex h e = hPutStrLn System.IO.stderr $ "Error writing to log handle " ++ (show h) ++ ": " ++ show e