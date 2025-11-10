{-# LANGUAGE OverloadedStrings #-}
module Blog.Widgets.JsonUtilities where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Vector as V
import Data.Scientific (toRealFloat, toBoundedInteger)
import qualified Codec.Binary.UTF8.String as UTF8

-- Parse UTF8-encoded JSON string
parse_utf8_json :: String -> Either String Value
parse_utf8_json s = eitherDecode $ LBS.pack $ UTF8.decodeString s

-- Path operator: extract value(s) by key
(</>) :: Value -> String -> Value
(Object o) </> s = flatten $ Array $ V.fromList $ map snd $ filter ((== K.fromText (T.pack s)) . fst) $ KM.toList o
(Array a) </> s = flatten $ Array $ V.map (\v -> v </> s) a
_ </> _ = Null

-- Flatten single-element arrays
flatten :: Value -> Value
flatten (Object o) = Object $ KM.map flatten o
flatten (Array v) | V.length v == 1 = flatten $ V.head v
flatten (Array v) = Array $ V.map flatten v
flatten y = y

-- Constants
blank :: Value
blank = String ""

zero :: Value
zero = Number 0

empty_object :: Value
empty_object = Object KM.empty

empty_array :: Value
empty_array = Array V.empty

-- Extract integer
unn :: Value -> Int
unn (Number n) = case toBoundedInteger n of
    Just i -> i
    Nothing -> round $ toRealFloat n
unn (Array v) | V.length v == 1 = unn $ V.head v
unn v = error $ "Can't un-number a non-Number value: " ++ (show v)

-- Extract integer array
unn_ :: Value -> [Int]
unn_ a@(Array _) = map unn $ una $ flatten a
unn_ r@(Number _) = [unn r]
unn_ v = error $ "Can't un-number-array a non-number-array value: " ++ (show v)

-- Extract integer with default
unnWithDefault :: Int -> Value -> Int
unnWithDefault _ j@(Number _) = unn j
unnWithDefault i (Array v) | V.length v == 1 = unnWithDefault i (V.head v)
unnWithDefault i _ = i

-- Extract object as key-value pairs
uno :: Value -> [(String, Value)]
uno (Object o) = map (\(k, v) -> (T.unpack (K.toText k), v)) $ KM.toList o
uno v = error $ "Can't un-object a non-Object value: " ++ (show v)

-- Extract array
una :: Value -> [Value]
una (Array a) = V.toList a
una v = [v]

-- Extract string
uns :: Value -> String
uns (String s) = T.unpack s
uns (Array v) | V.length v == 1 && isString (V.head v) = uns $ V.head v
  where isString (String _) = True
        isString _ = False
uns v = error $ "Can't un-string non-String value: " ++ (show v)

-- Extract string array
uns_ :: Value -> [String]
uns_ a@(Array _) = map uns $ una $ flatten a
uns_ (String s) = [T.unpack s]
uns_ n@(Number _) = [show $ unn n]
uns_ v = error $ "Can't un-string-array a non-Array value: " ++ (show v)

-- Extract string with default
unsWithDefault :: String -> Value -> String
unsWithDefault _ v@(String _) = uns v
unsWithDefault s (Array v) | V.length v == 1 = unsWithDefault s (V.head v)
unsWithDefault s _ = s
