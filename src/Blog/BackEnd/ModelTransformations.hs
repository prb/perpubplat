module Blog.BackEnd.ModelTransformations where

import Blog.Model.Entry -- unqualified.
import qualified Blog.BackEnd.IoOperations as I
import Utilities ( now )

import Data.Char
import Control.Concurrent ( forkIO )
import Text.ParserCombinators.Parsec

data IngestResults = IngestResults { item :: Maybe Item
                                   , model :: Model
                                   , success :: Bool
                                   , error_message :: Maybe String }

ingest_draft :: Item -> Model -> IO Model
ingest_draft i m = do { let t = to_permatitle $ title i
                      ; let pt = uniquify_permatitle m t 0
                      ; ts <- now
                      ; let i' = i { internal_id = next_id m
                                   , permatitle = pt
                                   , created = ts
                                   , updated = ts
                                   , kind = Post }
                      ; let (_,m') = insert m i'
                      ; forkIO $ I.save m' i' >> return ()
                      ; return m' }

-- | Add a comment to the model and storage system, including filling
-- in any necessary fields.  All comments are initially set to
-- invisible.
ingest_comment :: Item -> Model -> IO Model
ingest_comment c m = do { ts <- now
                        ; let int_id = next_id m
                        ; let new_comment = c { internal_id = int_id
                                              , updated = ts
                                              , permatitle = "comment-" ++ (show int_id)
                                              , kind = Comment }
                        ; let (_,m') = insert m new_comment
                        ; forkIO $ I.save m' new_comment >> return ()
                        ; return m' }

to_permatitle :: String -> String
to_permatitle = reverse . snip . reverse . snip . (squish_dashes "") . clean_chars . demarkup
    where
      snip = dropWhile $ (==) '-'

clean_chars :: String -> String
clean_chars [] = []
clean_chars (x:xs) = (t:(clean_chars xs))
    where
      t = case (generalCategory x) of
            Space -> '-'
            UppercaseLetter -> (toLower x)
            LowercaseLetter -> x
            DecimalNumber -> x
            _ -> '-'

squish_dashes :: String -> String -> String
squish_dashes s [] = reverse s
squish_dashes [] (x:xs) = squish_dashes [x] xs
squish_dashes s@(t:_) (x:xs) | (t == x) && (x=='-') = squish_dashes s xs
                              | otherwise = squish_dashes (x:s) xs

demarkup :: String -> String
demarkup t = case (parse demarkup_parser t t) of
               Left err -> error $ show err
               Right t' -> concat t'

demarkup_parser :: Parser [String]
demarkup_parser = many $ try element_parser
                  <|> try entity_parser
                  <|> nonmarkup_parser
 
nonmarkup_parser :: Parser String
nonmarkup_parser = many1 $ noneOf "&<"

element_parser :: Parser String
element_parser = char '<' >> (many $ noneOf ">") >> char '>' >> return ""

entity_parser :: Parser String
entity_parser = char '&' >> (many $ noneOf ";") >> char ';' >> return ""
                                                   
uniquify_permatitle :: Model -> String -> Int -> String
uniquify_permatitle m t n = if permatitle_exists m t' then
                                 uniquify_permatitle m t (n+1)
                             else 
                                 t'
    where
      t' = if n==0 then
               t
           else 
               t ++ "-" ++ (show n)
