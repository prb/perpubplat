{-

Reduced but acceptable profile of the Atom syndication format[1]
suitable for blog publishing.  Primary assumptions:

  * Place the onus on the user (i.e., the user of this API) to ensure
    that correct Atom is constructed.

  * Sidestep namespace qualification issues by just declaring the
    atom: prefix on the root feed element and then reusing it
    throughout.

  * Content comes in either the plain "text" flavor or the "xhtml"
    flavor.
 
  * The atom:generator structure always includes uri and version.
 
  * The atom:link structures only and always include rel and href
    attributes.
 
  * atom:contributor is not supported.
 
  * Entities are expected to be formatted correctly (text escaped,
    dates rendered, ids prepared) before being put in this structure.
    This includes attribute values.

  * atom:source is not supported.

  * Pretty printing is irrelevant.
 
REFERENCES:
  [1] http://www.atomenabled.org/developers/syndication/atom-format-spec.php

AUTHOR:
  prb@mult.ifario.us

COPYRIGHT:
  This file is in the public domain.

-}

module Text.Atom (AtomElement( Feed,Entry,Content, Author,
                              Category,Generator,Id,Icon,Link,
                              Logo,Published,Rights,Subtitle,Summary,Title,Updated,
                              author_name, author_uri, author_email, rel, href,
                              gen_name, gen_uri, gen_version
                             ),
                  AtomContent(AtomContent, contentType, body),
                  ContentType(XHTML,TEXT),
                  toXml,feed_link,feed_link_alt, start_feed, end_feed) where

import Lucid hiding (link_)
import qualified Data.Text as T
import Data.Maybe

{-
This is the main data structure.  As-is, the structure makes no effort
to enforce correct Atom structure (order/presence of elements),
although that could be done.  The onus is on the user of the API to
put things together correctly, which isn't too much to ask.
-}
data AtomElement = Feed [AtomElement]
                 | Entry [AtomElement]
                 | Content AtomContent
                 | Author { author_name :: String,
                            author_uri :: Maybe String,
                            author_email :: Maybe String }
                 | Category String
                 | Generator { gen_name :: String,
                               gen_uri :: String,
                               gen_version :: String }
                 | Id String
                 | Icon String
                 | Link { rel :: String,
                          href :: String }
                 | Logo String
                 | Published String
                 | Rights AtomContent
                 | Subtitle AtomContent
                 | Summary AtomContent
		 | Title AtomContent
                 | Updated String
                   deriving (Show) -- just for show; see toXml

{-
As with the other structures, the body content of an AtomContent
instance is expected to contain correctly formatted/escaped content,
i.e., entity-escaped "text" or valid "xhtml".  The implementation
assumes that the xhtml is a fragment and wraps it in the required
XHTML <div>.  (The div wrapper also sets the default namespace to
XHTML.)
-}
data AtomContent = AtomContent { contentType :: ContentType,
                                 body :: String }
                 deriving (Show) -- just for show; see toXml

-- Convenience enum for types of content
data ContentType = XHTML | TEXT
                 deriving (Eq,Show,Enum)

-- Render an AtomElement fragment as XML, no declaration. 
toXml :: AtomElement -> String
toXml (Feed xs) = wrap_ns "feed" (content_ xs)
toXml (Entry xs) = wrap_ns "entry" (content_ xs)

toXml' :: AtomElement -> String
toXml' (Entry xs) = wrap "entry" (content_ xs)
toXml' (Category s) = clopen "category" [("term",s)]
toXml' (Id s) = wrap "id" s
toXml' (Icon s) = wrap "icon" s
toXml' (Link r h) = clopen "link" [("rel",r),("href",h)]
toXml' (Logo s) = wrap "logo" s
toXml' (Published s) = wrap "published" s
toXml' (Updated s) = wrap "updated" s
toXml' (Author s u e) = wrap "author" ((wrap "name" s)
                                       ++ (wrap_m "uri" u)
                                       ++ (wrap_m "email" e))
toXml' (Generator n u v) = wrap_ "generator" [("uri",u),("version",v)] n
toXml' (Content a) = atom_text "content" a
toXml' (Rights a) = atom_text "rights" a
toXml' (Subtitle a) = atom_text "subtitle" a
toXml' (Summary a) = atom_text "summary" a
toXml' (Title a) = atom_text "title" a

content_ :: [AtomElement] -> String
content_ = concat.(map toXml')

-- Render an Atom text construct as XML.
atom_text :: String -> AtomContent -> String
atom_text s (AtomContent XHTML t) = wrap_ s [("type","xhtml")] (start_div ++ t ++ end_div)
atom_text s (AtomContent TEXT t) = wrap_ s [("type","text")] t

-- Format a clopen element with a list of attributes.
clopen :: String -> [(String,String)] -> String
clopen s [] = "<" ++ s ++ "/>"
clopen s xs = "<" ++ s ++ (nv_to_s "" xs) ++ "/>"

-- Wrap a string in an element.
wrap :: String -> String -> String
wrap s t = "<" ++ s ++ ">" ++ t ++ "</" ++ s ++ ">"

-- If a value is present (i.e., not Nothing), wrap it in an element.
wrap_m :: String -> Maybe String -> String
wrap_m _ Nothing = ""
wrap_m s (Just t) = wrap s t

-- Wrap an element with attributes around a string.
wrap_ :: String -> [(String,String)] -> String -> String
wrap_ s [] t = wrap s t
wrap_ s xs t = "<" ++ s ++ (nv_to_s "" xs) ++ ('>':t)
               ++ "</" ++ s ++ ">"

wrap_ns :: String -> String -> String
wrap_ns s t = wrap_ s [("xmlns",atom_uri)] t

-- Format a list of name-value pairs as attributes.
nv_to_s :: String -> [(String,String)] -> String
nv_to_s = foldl att

att :: String -> (String,String) -> String
att s (n,v) = s ++ (' ':(n ++ "=\"" ++ v ++ "\""))

atom_uri :: String
atom_uri = "http://www.w3.org/2005/Atom"

-- Convenience constants.
start_feed :: String
start_feed = "<feed xmlns=\"http://www.w3.org/2005/Atom\">"

end_feed :: String
end_feed = "</feed>"

-- | A @<div>@ with the XHTML namespace declared as text; used to wrap
-- entry content.
start_div :: String
start_div = "<div xmlns=\"http://www.w3.org/1999/xhtml\">"

-- | A @</div>@ as text; used to wrap entry content.
end_div :: String
end_div =  "</div>"

-- | Output a @rel=@-style autodiscovery link; see
-- <http://blog.whatwg.org/feed-autodiscovery>.
atom_link_ :: Bool -- ^ whether or not the feed is for the current document.
      -> String -- ^ the URL
      -> String -- ^ the title for the feed
      -> Html () -- ^ the link
atom_link_ alt u t = Lucid.link_ [ rel_ feed_kind, type_ "application/atom+xml"
                                  , href_ (T.pack u), title_ (T.pack t) ]
    where
      feed_kind = T.pack $ if alt then "feed alternate" else "feed"

-- | Convenience for creating a @rel=\"feed\"@ type @<link>@.
feed_link :: String -> String -> Html ()
feed_link = atom_link_ False

-- | Convenience for creating a @rel=\"feed alternate\"@ type @<link>@.
feed_link_alt :: String -> String -> Html ()
feed_link_alt = atom_link_ True