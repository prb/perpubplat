module Blog.FrontEnd.Syndication ( assemble_feed, last_updated ) where

import qualified Blog.Model.Entry as B
import qualified Blog.Constants as C
import qualified Blog.FrontEnd.Feeds as F
import qualified Text.Atom as A
import Data.Maybe (fromJust)

assemble_feed :: (F.Feedable f) => f -> B.Model -> [B.Item] -> String
assemble_feed g m items
    = A.toXml $ A.Feed ( [ A.Author C.author_name C.author_uri C.author_email ]
                         ++ (map A.Category categories)
                         ++ [ A.Generator C.generator_name C.generator_uri C.generator_version
                            , A.Id $ F.build_id g
                            , A.Link "self" $ F.self_url g
                            , A.Title $ A.AtomContent A.XHTML (F.title g)
                            , A.Updated l_u]
                         ++ map (to_entry m) items )
    where
      l_u = if (items == []) then
                C.first_datetime
            else 
                last_updated (B.flatten m items)
      categories = F.categories g

atomize_author :: B.Author -> A.AtomElement
atomize_author a = A.Author { A.author_name = anonymize_blank $ B.name a
                            , A.author_uri = B.uri a
                            , A.author_email = if B.show_email a then
                                                   B.email a
                                               else
                                                   Nothing }

anonymize_blank :: String -> String
anonymize_blank "" = C.anonymous_author
anonymize_blank s = s

last_updated :: [B.Item] -> String
last_updated [] = C.first_datetime
last_updated is = maximum $ map B.updated is

to_entry :: B.Model -> B.Item -> A.AtomElement
to_entry m i = A.Entry ( [ atomize_author $ B.author i ]
                       ++ ( map A.Category (B.tags i) )
                       ++ [ A.Content $ A.AtomContent A.XHTML (B.body i)
                          , A.Id ( plink )
                          , A.Link { A.rel = "alternate"
                                   , A.href = plink }
                          , A.Published $ B.created i ]
                         ++ atomize_summary i
                         ++ [ A.Title $ A.AtomContent A.XHTML (B.title i)
                            , A.Updated $ B.updated i ] )
    where
      plink = B.permalink m i

atomize_summary :: B.Item -> [ A.AtomElement ]
atomize_summary i | B.summary i == Nothing = []
                  | otherwise = [ A.Summary . (A.AtomContent A.XHTML) . fromJust $ B.summary i ]
