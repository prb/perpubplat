module Blog.Widgets.StreamOfConsciousness.GoogleReader ( start_google_reader, google_user ) where

import Blog.Constants ( google_user )
import Blog.Widgets.StreamOfConsciousness.Thought
import Blog.Widgets.StreamOfConsciousness.Controller
import Blog.BackEnd.HttpPoller
import Blog.Widgets.StreamOfConsciousness.RssUtilities ( textOf, maybeTextOf )

import qualified Text.XML.Light as TXL
import Network.HTTP.Client
import Data.Maybe ( fromJust )

import qualified Codec.Binary.UTF8.String as UTF8

google_reader_period :: Int
google_reader_period = 240 * 10^6

start_google_reader :: SoCController -> IO Worker
start_google_reader socc = do { let req = fromJust $ parseRequest $ "http://www.google.com/reader/public/atom/user/" ++ google_user ++ "/state/com.google/broadcast"
                                   ; p <- start_poller "GoogleReaderSharedItems" req (handle_posts socc) google_reader_period
                                   ; return $ Worker socc p }

handle_posts :: SoCController -> String -> IO ()
handle_posts socc = (commit socc) . fromAtom . UTF8.decodeString

fromAtom :: String -> [ Thought ]
fromAtom = (map fromAtomEntry) . concat . (map (TXL.findElements atom_entry))
           . TXL.onlyElems . TXL.parseXML

atom_uri :: Maybe String
atom_uri = Just "http://www.w3.org/2005/Atom"

atom_pfx :: Maybe String
atom_pfx = Nothing

atom_entry :: TXL.QName
atom_entry = TXL.QName "entry" atom_uri atom_pfx

atom_title :: TXL.QName
atom_title = TXL.QName "title" atom_uri atom_pfx

atom_updated :: TXL.QName
atom_updated = TXL.QName "updated" atom_uri atom_pfx

atom_link :: TXL.QName
atom_link = TXL.QName "link" atom_uri atom_pfx

gr_uri :: Maybe String
gr_uri = Just "http://www.google.com/schemas/reader/atom/"

gr_pfx :: Maybe String
gr_pfx = Just "gr"

gr_annotation :: TXL.QName
gr_annotation = TXL.QName "annotation" gr_uri gr_pfx

-- Google Reader's feed reuses this element for the annotation.
atom_content :: TXL.QName
atom_content = TXL.QName "content" atom_uri atom_pfx

fromAtomEntry :: TXL.Element -> Thought
fromAtomEntry e = Thought GoogleReader (textOf atom_updated e) (relAlternateLink e)
                  (textOf atom_title e) (annotation e)


relAlternateLink :: TXL.Element -> String
relAlternateLink e =  case filter rel_alt links of
                        [] -> ""
                        (e':_) -> href e'                        
    where
      links = TXL.findChildren atom_link e
      rel_alt = hasUnQAttrWithValue "rel" "alternate"

href :: TXL.Element -> String
href e = case TXL.findAttr (unQName "href") e of
           Nothing -> ""
           Just s -> s

annotation :: TXL.Element -> Maybe String
annotation e = case TXL.findChild gr_annotation e of
                 Nothing -> Nothing
                 Just e' -> maybeTextOf atom_content e'

hasUnQAttrWithValue :: String -> String -> TXL.Element -> Bool
hasUnQAttrWithValue n v = (elem $ TXL.Attr (unQName n) v) . TXL.elAttribs

unQName :: String -> TXL.QName
unQName s = TXL.QName s Nothing Nothing