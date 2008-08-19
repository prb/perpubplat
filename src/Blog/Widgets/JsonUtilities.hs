module Blog.Widgets.JsonUtilities where

import Text.JSON
import qualified Codec.Binary.UTF8.String as UTF8

parse_utf8_json :: String -> Either String JSValue
parse_utf8_json = resultToEither . decode . UTF8.decodeString

(</>) :: JSValue -> String -> JSValue
(JSObject o) </> s = flatten . JSArray $ map snd $ filter (((==) s) . fst) $ fromJSObject o
(JSArray a) </> s = flatten . JSArray $ map (flip (</>) $ s) a
_ </> _ = JSNull

flatten :: JSValue -> JSValue
flatten (JSObject o) = JSObject $ toJSObject $ map (\(s,v) -> (s,flatten v)) $ fromJSObject o
flatten (JSArray [x]) = flatten x
flatten (JSArray a) = JSArray $ map flatten a
flatten y = y

blank :: JSValue
blank = JSString $ toJSString ""

zero :: JSValue
zero = JSRational 0

empty_object :: JSValue
empty_object = JSObject $ toJSObject []

empty_array :: JSValue
empty_array = JSArray []

unn_ :: JSValue -> [Int]
unn_ a@(JSArray _) = map unn $ una . flatten $ a

unnWithDefault :: Int -> JSValue -> Int
unnWithDefault i JSNull = i
unnWithDefault i (JSArray []) = i
unnWithDefault i (JSArray [j]) = unnWithDefault i j
unnWithDefault _ j = unn j

unn :: JSValue -> Int
unn (JSRational n) = fromInteger . round $ n
unn (JSArray [n@(JSRational _)]) = unn n

uno :: JSValue -> [(String,JSValue)]
uno (JSObject o) = fromJSObject o
uno _ = error "Can't un-object a non-JSOBject value."

una :: JSValue -> [JSValue]
una (JSArray a) = a
una _ = error "Can't un-array a non-JSArray value."

uns_ :: JSValue -> [String]
uns_ a@(JSArray _) = map uns $ una . flatten $ a

uns :: JSValue -> String
uns (JSString s) = fromJSString s
uns (JSArray [s@(JSString _)]) = uns s
uns _ = error "Can't un-string non-JSString value."

unsWithDefault :: String -> JSValue -> String
unsWithDefault s JSNull = s
unsWithDefault s (JSArray []) = s
unsWithDefault s (JSArray [v]) = unsWithDefault s v
unsWithDefault _ v = uns v