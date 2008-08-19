module Blog.FrontEnd.Actions where

data Action = Ingest { draft :: String }
            deriving ( Show, Eq )
