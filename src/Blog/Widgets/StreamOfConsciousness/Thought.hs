module Blog.Widgets.StreamOfConsciousness.Thought where

import Text.XHtml.Strict
import Data.List

import Blog.FrontEnd.ContentAtoms
import qualified Blog.Constants as C

data Channel = Delicious | TwitterTweet | TwitterReply | GoogleReader | Identica
             deriving ( Show, Read, Eq, Ord, Enum )

icon :: Channel -> Html
icon c = image ! [ src $ C.blog_root ++ "/files/" ++ (show c) ++ "-icon.png"
               , alt $ show c ]

data Thought = Thought { channel :: Channel
                       , date :: String
                       , url :: String
                       , txt :: String
                       , desc :: Maybe String }

               deriving ( Show, Read )

instance Eq Thought where
    t1 == t2 = (channel t1 == channel t2)
               && (date t1 == date t2)

instance Ord Thought where
    t1 <= t2 = ((date t1) <= (date t2))
               || ( ((date t1) == (date t2)) 
                    && ((channel t1) <= (channel t2)) )

thoughts_to_xhtml :: [Thought] -> String
thoughts_to_xhtml = showHtmlFragment . (divid "thoughts")
                    . (traverse_thoughts "1970-01-01")

traverse_thoughts :: String -> [Thought] -> Html
traverse_thoughts _ [] = noHtml
traverse_thoughts l_d (t:ts)
    = (concatHtml [ if l_d `isPrefixOf` d then
                       noHtml
                   else
                       (p ! [theclass "thought_group" ]) . stringToHtml $ d
                  , p ! [theclass "thought"] $ concatHtml [ icon $ channel t
                                                          , stringToHtml " "
                                                          , to_html t
                                                          ]
                  ]) +++ (traverse_thoughts d ts)
      where
        d = take 10 $ date t

to_html :: Thought -> Html
to_html (Thought k d u t _) | k == TwitterTweet || k == TwitterReply || k == Identica
                                      = concatHtml [ _a u time
                                                   , stringToHtml " "
                                                   , text ]
                          where
                            wrap st = (thespan ! [ theclass st ])
                            time_hunk = (take 9) . (drop 11)
                            time = (wrap "tweet_stamp") . stringToHtml $ time_hunk d
                            text = wrap "tweet_text" $ primHtml t

to_html (Thought _ _ u t Nothing) = _a u (primHtml t)
to_html (Thought _ _ u t (Just d)) = _a u $ concatHtml [ _a u (primHtml t) 
                                                       , stringToHtml " "
                                                       , stringToHtml d ]

-- "zip" two lists into an ordered list, eliminating duplicates
merge :: (Eq a, Ord a) => [a] -> [a] -> [a]
merge as bs = merge_ as [] bs

merge_ :: (Eq a, Ord a) => [a] -> [a] -> [a] -> [a]
merge_ [] y z = (reverse y) ++ z
merge_ x y [] = (reverse y) ++ x
merge_ x@(x0:xs) y z@(z0:zs) | x0 == z0 = merge_ xs (x0:y) zs
                             | x0 >= z0 = merge_ xs (x0:y) z
                             | otherwise = merge_ x (z0:y) zs

dedup :: (Eq a) => [a] -> [a]
dedup = (map head) . group