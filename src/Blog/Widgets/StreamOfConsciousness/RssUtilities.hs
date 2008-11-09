module Blog.Widgets.StreamOfConsciousness.RssUtilities where

import qualified Text.XML.Light as TXL
import Utilities ( httpDateToIso8601 )
import qualified Blog.Widgets.StreamOfConsciousness.XmlUtilities as XU
import Blog.Widgets.StreamOfConsciousness.Thought ( Thought (..), Channel )

rss2_item :: TXL.QName
rss2_item = TXL.QName "item" Nothing Nothing

rss2_title :: TXL.QName
rss2_title = TXL.QName "title" Nothing Nothing

rss2_link :: TXL.QName
rss2_link = TXL.QName "link" Nothing Nothing

rss2_description :: TXL.QName
rss2_description = TXL.QName "description" Nothing Nothing

rss2_pubDate :: TXL.QName 
rss2_pubDate = TXL.QName "pubDate" Nothing Nothing

dc_uri :: Maybe String
dc_uri = Just "http://purl.org/dc/elements/1.1/"

dc_date :: TXL.QName 
dc_date = TXL.QName "date" dc_uri $ Just "dc"

fromRSS2 :: Channel -> String -> [Thought]
fromRSS2 ch = (map (fromRSS2Item ch)) . concat . (map (TXL.findElements rss2_item))
              . TXL.onlyElems . TXL.parseXML

fromRSS2Item :: Channel -> TXL.Element -> Thought
fromRSS2Item ch e = Thought ch (httpDateToIso8601 $ textOf rss2_pubDate e) (textOf rss2_link e) (textOf rss2_title e)
                    (maybeTextOf rss2_description e)

rss1_uri :: Maybe String
rss1_uri = Just "http://purl.org/rss/1.0/"

rss1_pfx :: Maybe String
rss1_pfx = Just "rss"

rss1_item :: TXL.QName
rss1_item = TXL.QName "item" rss1_uri rss1_pfx

rss1_title :: TXL.QName
rss1_title = TXL.QName "title" rss1_uri rss1_pfx

rss1_link :: TXL.QName
rss1_link = TXL.QName "link" rss1_uri rss1_pfx

rss1_description :: TXL.QName
rss1_description = TXL.QName "description" rss1_uri rss1_pfx

fromRSS1 :: Channel -> String -> [Thought]
fromRSS1 ch = (map (fromRSS1Item ch)) . concat . (map (TXL.findElements rss1_item))
              . TXL.onlyElems . TXL.parseXML

fromRSS1Item :: Channel -> TXL.Element -> Thought
fromRSS1Item ch e = Thought ch (textOf dc_date e) (textOf rss1_link e) (textOf rss1_title e) (maybeTextOf rss1_description e)

textOf :: TXL.QName -> TXL.Element -> String
textOf q e = case (TXL.findElement q e) of
               Nothing -> ""
               Just e' -> XU.substituteEntities . TXL.strContent $ e'

maybeTextOf :: TXL.QName -> TXL.Element -> Maybe String
maybeTextOf q e = case (TXL.findElement q e) of
                    Nothing -> Nothing
                    Just e' -> Just . XU.substituteEntities . TXL.strContent $ e'

