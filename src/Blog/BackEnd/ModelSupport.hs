module Blog.BackEnd.ModelSupport where

import Blog.Model.Entry (Item, Model)
import qualified Blog.BackEnd.ModelTransformations as MT
import qualified Blog.BackEnd.IoOperations as IoO
import qualified Blog.BackEnd.Holder as H

import Control.Monad ((=<<))

boot :: IO (H.Holder Model)
boot = H.newHolder =<< IoO.boot

ingest_draft :: (H.Holder Model) -> String -> IO (Either String Item)
ingest_draft h s = do { d <- IoO.load_draft s
                      ; case d of
                          Right i -> 
                              do { H.applyIO' h (MT.ingest_draft i)
                                 ; return $ Right i }
                          Left err -> 
                              return $ Left $ show err }

post_comment :: (H.Holder Model) -> Item -> IO ()
post_comment h i = do H.applyIO' h (MT.ingest_comment i)
