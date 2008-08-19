module Blog.Model.EntryParser where

import Text.ParserCombinators.Parsec
import qualified Blog.Model.Entry as B

item_from_string :: String -> String -> Either (String,String) B.Item
item_from_string = fromString parse_item

draft_from_string :: String -> String -> Either (String,String) B.Item
draft_from_string = fromString parse_draft

fromString :: Parser B.Item -> String -> String -> Either (String,String) B.Item
fromString p path datum = case (parse p path datum) of
                            Left err -> Left (path, show err)
                            Right item -> Right item

parse_draft :: Parser B.Item
parse_draft = do { title <- parse_field "title"
                 ; tags <- parse_tags
                 ; body <- parse_block "BODY"
                 ; summary <- parse_summary
                 ; return $ B.Item 0 B.Post title summary body tags
                   "uid" "permatitle" "created" "updated" B.default_author True Nothing }

parse_item :: Parser B.Item
parse_item = do { int_id <- parse_field "internal_id"
                ; parent <- parse_field "parent"
                ; title <- parse_field "title"
                ; tags <- parse_tags
                ; ptitle <- parse_field "permatitle"
                ; kind <- parse_field "kind"
                ; uid <- parse_field "uid"
                ; created <- parse_field "created"
                ; updated <- parse_field "updated"
                ; author <- parse_field "author"
                ; visible <- parse_field "visible"
                ; body <- parse_block "BODY"
                ; summary <- parse_summary
                ; return $ B.Item (read int_id) (read kind) title summary
                  body tags uid ptitle created updated (read author)
                  (read visible) (read parent) }

parse_field :: String -> Parser String
parse_field s = do { string s
                   ; char ':'
                   ; skipMany (char ' ' <|> char '\t')
                   ; v <- manyTill anyChar newline
                   ; return v }

parse_tags :: Parser [String]
parse_tags = do { string "tags:"
                ; skipMany (char ' ' <|> char '\t')
                ; tags <- parse_tag `sepBy` (many1 $ oneOf ", ")
                ; newline
                ; return tags }

parse_tag :: Parser String
parse_tag = many1 (letter <|> digit <|> (oneOf "_-."))
            <?> "Expected a combination of letters, digits, -. and _"

parse_block :: String -> Parser String
parse_block s = do { skipMany newline
                   ; string $ concat ["--- START ", s, " ---\n"]
                   ; manyTill anyChar (try $ end_block s) }

end_block :: String -> Parser ()
end_block s = do { string $ concat [ "\n--- END ", s, " ---" ]
                 ; many $ char '\n'
                 ; return () }

parse_summary :: Parser (Maybe String)
parse_summary = (try $ do { s <- parse_block "SUMMARY"
                          ; return $ Just s  } )
                <|> return Nothing