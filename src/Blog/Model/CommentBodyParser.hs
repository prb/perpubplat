module Blog.Model.CommentBodyParser where

import Blog.FrontEnd.ContentAtoms

import Text.ParserCombinators.Parsec
import Text.XHtml.Strict

import Data.List (intersperse) 

data Block = Code { lines :: [String] }
           | Quote { lines :: [String] }
           | Paragraph { hunks :: [Block] }
           | Text { text :: String }
           | Link { url :: String, text :: String }
             deriving ( Show, Eq, Ord )

convert_comment_body :: String -> Html
convert_comment_body s = case (parse parse_comment_body "<comment body>" s) of
                           Left err ->
                               concatHtml [ p . concatHtml $ [ bold << stringToHtml "Parsing Error: "
                                                             , stringToHtml . show $ err ]
                                          , pre << stringToHtml s ]
                           Right b ->
                               blocks_to_xhtml b

parse_comment :: String -> Either ParseError [Block]
parse_comment = parse parse_comment_body "<internal>"

blocks_to_xhtml :: [Block] -> Html
blocks_to_xhtml b = concatHtml . (map render) $ collect_ b []

blocks_to_string :: [Block] -> String
blocks_to_string = showHtmlFragment . blocks_to_xhtml

render :: Block -> Html
render (Code a) = ( pre ! [ theclass "code" ] ) . concatHtml . (intersperse (stringToHtml "\n")) . (map stringToHtml) $ a
render (Quote a) = blockquote . concatHtml . (map ( p . stringToHtml )) $ a
render (Paragraph a) = p . concatHtml . (map render) $ a
render (Text a) = stringToHtml a
render (Link u t) = _at u t

collect_ :: [Block] -> [Block] -> [Block]
collect_ [] s = reverse s
collect_ (a:as) [] = collect_ as [a]
collect_ (a:as) y@(b:bs) = case (a,b) of
                            (Code l, Code m) -> collect_ as ((Code (m ++ l)):bs)
                            (Quote q, Quote r) -> collect_ as ((Quote (r ++ q)):bs)
                            _ -> collect_ as (a:y)

parse_comment_body :: Parser [Block]
parse_comment_body = sepEndBy parse_block eol

parse_block :: Parser Block
parse_block = parse_code_block
              <|> parse_quote_block
              <|> parse_paragraph_block

parse_code_block :: Parser Block
parse_code_block = do { char '>'
                      ; s <- many not_eol
                      ; return $ Code [s] }

parse_quote_block :: Parser Block
parse_quote_block = do { char '|'
                       ; s <- many not_eol
                       ; return $ Quote [s] }

parse_paragraph_block :: Parser Block
parse_paragraph_block = do { b <- many1 ( parse_link_block <|> parse_text_block )
                           ; return $ Paragraph b }

parse_link_block :: Parser Block
parse_link_block = do { start_link
                      ; s <- (many1 $ noneOf [ '^','|','\n','\r' ]) <?> "display text for the link"
                      ; char '|' <?> "a pipe to separate the displayed text and the URL"
                      ; t <- (many1 $ uriChar) <?> "a URL"
                      ; end_link <?> "a \"^\" to terminate the link"
                      ; return $ Link s t }

parse_text_block :: Parser Block
parse_text_block = do { s <- many1 $ noneOf ['^','\r','\n']
                      ; return $ Text s }

uriChar :: Parser Char
uriChar = unreserved <|> gen_delim <|> sub_delim

gen_delim :: Parser Char
gen_delim = oneOf ":/?#[]@"

sub_delim :: Parser Char
sub_delim = oneOf "!$&'()*+,;=" 

unreserved :: Parser Char
unreserved = letter <|> digit <|> oneOf "-._~"

start_link :: Parser ()
start_link = do { char '^'
                ; spaces }

end_link :: Parser Char
end_link = do { spaces
              ; char '^' }

not_eol :: Parser Char
not_eol  = noneOf $ [ '\n','\r' ]

eol :: Parser ()
eol = skipMany1 . oneOf $ ['\n', '\r']