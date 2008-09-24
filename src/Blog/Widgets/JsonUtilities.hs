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
unn_ r@(JSRational _) = [ unn r ]
unn_ v = error $ "Can't un-number-array a non-number-array value: " ++ (show v)

unnWithDefault :: Int -> JSValue -> Int
unnWithDefault _ j@(JSRational _) = unn j
unnWithDefault i (JSArray [j]) = unnWithDefault i j
unnWithDefault i _ = i

unn :: JSValue -> Int
unn (JSRational n) = fromInteger . round $ n
unn (JSArray [n@(JSRational _)]) = unn n
unn v = error $ "Can't un-number a non-JSRational value: " ++ (show v)

uno :: JSValue -> [(String,JSValue)]
uno (JSObject o) = fromJSObject o
uno v = error $ "Can't un-object a non-JSOBject value: " ++ (show v)

una :: JSValue -> [JSValue]
una (JSArray a) = a
una v = error $ "Can't un-array a non-JSArray value: " ++ (show v)

uns_ :: JSValue -> [String]
uns_ a@(JSArray _) = map uns $ una . flatten $ a
uns_ (JSString s) = [fromJSString s]
uns_ n@(JSRational _) = [show . unn $ n]
uns_ v = error $ "Can't un-string-array a non-JSArray value: " ++ (show v)

uns :: JSValue -> String
uns (JSString s) = fromJSString s
uns (JSArray [s@(JSString _)]) = uns s
uns v = error $ "Can't un-string non-JSString value: " ++ (show v)

unsWithDefault :: String -> JSValue -> String
unsWithDefault _ v@(JSString _) = uns v
unsWithDefault s (JSArray [v]) = unsWithDefault s v
unsWithDefault s _ = s