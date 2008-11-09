module Blog.Widgets.StreamOfConsciousness.XmlUtilities ( substituteEntities ) where

import qualified Data.Char as DC
import Text.ParserCombinators.Parsec
import qualified System.Log.Logger as L

log_handle :: String
log_handle = "XmlUtilities"

substituteEntities :: String -> IO String
substituteEntities s
    = case parse entityParser "" s of
        Left err -> do { L.errorM log_handle $ "Unable to process entitles from malformed input string: " ++ s
                      ; return s }
        Right v -> return v

entityParser :: Parser String
entityParser = many (try entity <|> anyChar)

entity :: Parser Char
entity = (try ampersand)
         <|> (try quote)
         <|> (try apostrophe)
         <|> (try lessThan)
         <|> (try greaterThan)
         <|> (try decimalEntity)
         <|> hexEntity

ampersand :: Parser Char
ampersand = do { string "&amp;"
               ; return '&' }

quote :: Parser Char
quote = do { string "&quot;"
           ; return '"'}

apostrophe :: Parser Char
apostrophe = do { string "&apos;"
                ; return '\'' }

lessThan :: Parser Char
lessThan = do { string "&lt;"
              ; return '<' }

greaterThan :: Parser Char
greaterThan = do { string "&gt;"
                 ; return '>' }

decimalEntity :: Parser Char
decimalEntity = do { string "&#"
                   ; n <- many1 digit
                   ; char ';'
                   ; return $ DC.chr $ read_dec 0 n }

read_dec :: Int -> String -> Int
read_dec n "" = n
read_dec n (c:s) = read_dec ((10 * n) + (DC.ord c) - (DC.ord '0')) s

read_hex :: Int -> String -> Int
read_hex n "" = n
read_hex n (c:s) = read_hex ((16 * n) + (val $ DC.ord c)) s
    where
      val c | c > 96 && c < 102 = c - (DC.ord 'a')
            | c > 64 && c < 71 = c - (DC.ord 'A')
            | otherwise = c - (DC.ord '0')

hexEntity :: Parser Char
hexEntity = do do { string "&#x"
                  ; n <- many1 hexDigit
                  ; char ';'
                  ; return $ DC.chr $ read_hex 0 n }

