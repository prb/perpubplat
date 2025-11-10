module Blog.Model.CommentForm where

import qualified Blog.Model.Entry as B
import qualified Blog.Model.CommentBodyParser as CBP
import Utilities

import Data.List (dropWhile)
import Data.Char (isSpace) 

data CommentForm = CommentForm { authorName :: Field 
                               , authorEmail :: Field
                               , authorUri :: Field
                               , body :: Field }
                   deriving ( Show, Read, Eq, Ord )

data Field = Field { title :: String,
                     value :: String,
                     message :: Maybe String }
             deriving ( Show, Read, Eq, Ord )

new_comment_form :: String -> String -> String -> String -> CommentForm
new_comment_form n e u b = CommentForm (Field "name" n Nothing)
                           (Field "email" e Nothing) (Field "URL" u Nothing) (Field "body" b Nothing)

from_s :: String -> Field -> Field
from_s s f = f { value = s, message = Nothing }

from_item :: B.Item -> CommentForm
from_item i = new_comment_form (B.name a)
              (blankIfNothing . B.email $ a) (blankIfNothing . B.uri $ a)
              (B.body i)
    where
      a = B.author i
          
to_item :: B.Item -> CommentForm -> IO B.Item
to_item i cf = do { ts <- now
                  ; return $
                    B.Item 0 B.Comment "" Nothing (value . body $ cf)
                         [] "" "" ts ts
                         ( B.Author ( value . authorName $ cf )
                           ( Just $ value . authorUri $ cf )
                           ( Just $ value . authorEmail $ cf )
                           False )
                         True (Just $ B.internal_id i) }

validate :: CommentForm -> (CommentForm,Bool)
validate (CommentForm n e u b) = (result, err)
    where 
      result = validate_body $ CommentForm (require_not_blank n) (require_not_blank e) (require_not_blank u) (require_not_blank b)
      err = is_form_valid result

validate_body :: CommentForm -> CommentForm
validate_body cf = case CBP.parse_comment . value . body $ cf of
                     Right _ ->
                         cf
                     Left err ->
                         cf { body = (body cf) { message = Just $ show err } }

is_form_valid :: CommentForm -> Bool
is_form_valid (CommentForm n e u b) = (is_field_valid n) && (is_field_valid e)
                                      && (is_field_valid u) && (is_field_valid b)

is_field_valid :: Field -> Bool
is_field_valid (Field _ _ Nothing) = True
is_field_valid _ = False

require_not_blank :: Field -> Field
require_not_blank f = require_not_blank_ $ f { value = trim . value $ f }

require_not_blank_ :: Field -> Field
require_not_blank_ (Field n v _) | length v == 0 = Field n "" $ Just $ "The " ++ n ++ " field may not be empty."
require_not_blank_ (Field n v _) = Field n v Nothing

blankIfNothing :: Maybe String -> String
blankIfNothing Nothing = ""
blankIfNothing (Just s) = s

trim :: String -> String
trim = reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace)