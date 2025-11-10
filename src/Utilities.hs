module Utilities ( subst, hotness, readFile', now, format_time
                 , map_by, paginate, last_page, days_since
                 , iso8601toRfc1123, httpDateToIso8601
                 , tenths_of_a_second, elapsed_hundreths
                 , show_rc
                 , unr, unl, isr, isl, rights, lefts ) where

import qualified Data.Time.Calendar as C
import qualified Data.Time.Format as DTF
import qualified Data.Time.Clock.POSIX as POSIX
import Text.ParserCombinators.Parsec as P
import Data.List
import System.IO
import System.IO.Unsafe
import Foreign
import Data.Char
import qualified Data.Time.Clock as DTC
import qualified Data.Map as M

unr :: Either a b -> b
unr (Right r) = r
unr (Left _) = error "Can't unright a left."

unl :: Either a b -> a
unl (Right _) = error "Can't unleft a right."
unl (Left l) = l

isr :: Either a b -> Bool
isr (Right _) = True
isr (Left _) = False

isl :: Either a b -> Bool
isl (Right _) = False
isl (Left _) = True

rights :: [Either a b] -> [b]
rights = (map unr) . (filter isr)

lefts :: [Either a b] -> [a]
lefts = (map unl) . (filter isl)

paginate :: Int -- ^ the page size
      -> Int -- ^ the page index, starting from 1
      -> [a] -- ^ the list to page through
      -> [a]
paginate _ _ [] = []
paginate s i l | i < 1 = paginate s 1 l
               | i <= last_page s l = take s (drop (s*(i-1)) l)
               | otherwise = paginate s (last_page s l) l
                                 
last_page :: Int -> [a] -> Int
last_page _ [] = 1
last_page s as = (((length as) - 1) `div` s) + 1

data KMP a = KMP
      { done :: Bool
      , next :: (a -> KMP a)
      }

range :: (a->b) -> [a] -> [(b,a)]
range f is = zip (map f is) is

map_by :: (Ord b) => (a -> b) -> [a] -> M.Map b a
map_by f is = M.fromList $ range f is


-- | KMP-like implementation of string containment.  Pulled from 
--  <http://twan.home.fmf.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell.details>
subst :: String -- ^ the string to search for
      -> String -- ^ the string to search in
      -> Bool
subst as bs = any done $ scanl next (makeTable as) bs

makeTable :: Eq a => [a] -> KMP a
makeTable xs = table
   where table = makeTable' xs (const table)

makeTable' []     failure = KMP True failure
makeTable' (x:xs) failure = KMP False test
   where  test  c = if c == x then success else failure c
          success = makeTable' xs (next (failure x))

-- | Function to compute an HTML color value (three 0x00-0xFF hex digits
-- of RGB color space) from a [0,1] temperature.  The implementation is a
-- close approximation of a line connecting (0,100,100) and (270,100,70)
-- in HSB color space where one maps to the bright red (0,100,100) and zero
-- maps to the cool purple.
hotness :: Double -- ^ the hotness
        -> String
hotness a | a < 0 = error "Can't be cooler than zero."
hotness a | a > 1 = error "Can't be hotter than one."
hotness a = rgb_to_htmlcolor ((r x) + (rs x), g x, b x)
    where
      x = round(shape(a) * 270)
      r = lump (-120,-60,60,120) 255
      g = lump (0,60,180,240) 195
      b = lump (120,180,300,360) 135
      rs = lump (240,300,420,480) 255

-- Derived from (1/(5*a+1))-(a/6)
shape :: Double -> Double
shape a = (6-a-5*a*a)/(30*a+6)

-- | Ramp up, hold, ramp down and zero elsewhere function. 
lump :: (Int,Int,Int,Int) -- ^ four-tuple consisting of start of up ramp, end of up ramp, start of down ramp, end of down ramp
     -> Int -- ^ height for the hold portion
     -> Int -- ^ the input to the function
     -> Int
lump (su,eu,sd,ed) h x | x < su || x > ed = 0
                       | x > eu && x < sd = h
                       | x >= su && x <= eu = (h*(x-su)) `div` (eu-su)
                       | x >= sd && x <= ed = (h*(ed-x)) `div` (ed-sd)

digits :: String
digits = "0123456789ABCDEF"

hex :: [String]
hex = [ [digits!!a,digits!!b] | a <- [0..15], b <- [0..15] ]

rgb_to_htmlcolor :: (Int,Int,Int) -> String
rgb_to_htmlcolor (a,b,c) = "#" ++ (hex!!(clip a))
                           ++ (hex!!(clip b)) ++ (hex!!(clip c))
    where
      clip = \x -> min (max x 0) 255


readFile' f = do
  h <- openFile f ReadMode
  s <- hFileSize h
  fp <- mallocForeignPtrBytes (fromIntegral s)
  len <- withForeignPtr fp $ \buf -> hGetBuf h buf (fromIntegral s)
  lazySlurp fp 0 len

buf_size = 4096 :: Int

lazySlurp :: ForeignPtr Word8 -> Int -> Int -> IO String
lazySlurp fp ix len
  | fp `seq` False = undefined
  | ix >= len = return []
  | otherwise = do
      cs <- unsafeInterleaveIO (lazySlurp fp (ix + buf_size) len)
      ws <- withForeignPtr fp $ \p -> loop (min (len-ix) buf_size - 1) 
					((p :: Ptr Word8) `plusPtr` ix) cs
      return ws
 where
  loop :: Int -> Ptr Word8 -> String -> IO String
  loop len p acc
    | len `seq` p `seq` False = undefined
    | len < 0 = return acc
    | otherwise = do
       w <- peekElemOff p len
       loop (len-1) p (chr (fromIntegral w):acc)

now :: IO String
now = do { utc <- DTC.getCurrentTime
         ; return $ format_time utc }

format_time :: DTC.UTCTime -> String
format_time = DTF.formatTime DTF.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

days_since :: String -> IO Int
days_since d = do { n <- now
                  ; return . fromInteger $ C.diffDays (toDay n) (toDay d) }

iso8601toRfc1123 :: String -> String
iso8601toRfc1123 = (flip (++) $ " GMT") . (DTF.formatTime DTF.defaultTimeLocale rfc1123DateFormat) . iso8601toUTCTime

iso8601toUTCTime :: String -> DTC.UTCTime
iso8601toUTCTime ts = DTC.UTCTime (toDay ts) (toSecondsSinceMidnight ts)

toDay :: String -> C.Day
toDay ts = C.fromGregorian _y _m _d
    where
      _y = read . (take 4) $ ts
      _m = read . (take 2) . (drop 5) $ ts
      _d = read . (take 2) . (drop 8) $ ts

toSecondsSinceMidnight :: String -> DTC.DiffTime
toSecondsSinceMidnight ts = DTC.secondsToDiffTime $ _h * 3600 + _m * 60 + _s
    where
      ts' = drop 11 ts
      _h = read . (take 2) $ ts'
      _m = read . (take 2) . (drop 3) $ ts'
      _s = read . (take 2) . (drop 6) $ ts'

zpad :: String -> String
zpad [] = "00"
zpad [a] = '0':[a]
zpad s = s

rfc1123DateFormat :: String
rfc1123DateFormat = "%a, %d %b %Y %T" -- and then a " GMT" on the end

httpDateToIso8601 :: String -> String
httpDateToIso8601 d = case P.parse httpDateParser d d of 
                        Left e -> show e
                        Right d' -> d'

httpDateParser :: P.Parser String
httpDateParser = (try rfc1123DateParser)
                 <|> (try rfc850DateParser) <|> ansiDateParser

ansiDateParser :: P.Parser String
ansiDateParser = do { short_day
                    ; space
                    ; month <- short_month
                    ; space
                    ; spaces
                    ; day <- many1 digit
                    ; space
                    ; t <- hms
                    ; space
                    ; year <- count 4 digit
                    ; return $ concat [ year, "-", (zpad $ show month), "-", zpad day, "T"
                                      , t, "Z" ] }

rfc850DateParser :: P.Parser String
rfc850DateParser = do { long_day
                      ; string ", "
                      ; day <- count 2 digit
                      ; char '-'
                      ; month <- short_month
                      ; char '-'
                      ; year <- count 2 digit
                      ; space
                      ; t <- hms
                      ; string " GMT"
                      ; return $ concat [ "20", year, "-", (zpad $ show month), "-", day, "T"
                                        , t, "Z" ] }

rfc1123DateParser :: P.Parser String
rfc1123DateParser = do { short_day
                       ; string ", "
                       ; day <- count 2 digit
                       ; space
                       ; month <- short_month
                       ; space 
                       ; year <- count 4 digit
                       ; space
                       ; t <- hms
                       ; space
                       ; (try $ string "GMT") <|> (string "+0000")
                       ; return $ concat [ year, "-", (zpad $ show month), "-", day, "T"
                                         , t, "Z" ] }

hms :: P.Parser String
hms = do { hour <- count 2 digit
         ; char ':'
         ; minute <- count 2 digit
         ; char ':'
         ; seconds <- count 2 digit 
         ; return $ concat [ hour, ':':minute, ':':seconds ]}

long_day :: P.Parser String
long_day = (P.choice $ map (try . P.string) [ "Sunday", "Monday", "Tuesday", "Wednesday"
                                               , "Thursday", "Friday", "Saturday" ])

short_day :: P.Parser String
short_day = P.choice $ map (try . P.string) [ "Sun", "Mon", "Tue", "Wed", "Thu"
                                            , "Fri","Sat" ]

short_month :: P.Parser Int
short_month = P.choice $ map (\(n,s) -> (try . P.string $ s) >> return n)
              $ zip [1..12] [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
                            , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]

tenths_of_a_second :: DTC.NominalDiffTime -> String
tenths_of_a_second diff = fmt $ show hundreths
    where
      totalSeconds = truncate diff :: Integer
      picoseconds = truncate $ (diff - fromInteger totalSeconds) * 1e12
      minutes = totalSeconds `div` 60
      seconds = totalSeconds `mod` 60
      hundreths = 6000 * minutes + 100 * seconds + (picoseconds `div` (10^10))

elapsed_hundreths :: DTC.UTCTime -> DTC.UTCTime -> String
elapsed_hundreths ct_stop ct_start = tenths_of_a_second $ DTC.diffUTCTime ct_stop ct_start

fmt :: String -> String
fmt s = (take l ps) ++ "." ++ ((drop l) ps)
    where
      ps = pad 3 '0' s
      l = (length ps) - 2

pad :: Int -> Char -> String -> String
pad i c s | length s >= i = s
          | otherwise = pad (i-1) c (c:s)

show_rc :: (Int, Int, Int) -> String
show_rc (h,t,o) = [ digit h, digit t, digit o ]
    where
      digit = chr . (ord '0' + )
