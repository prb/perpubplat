module UtilitiesTests where

import qualified Utilities as U
import Time
import Test.HUnit.Base
import Control.Monad ( liftM )

testUtilities = TestList [ testPaginate
                         , testLastPage
                         , testTimeFormatting
                         , testTimestampTransformations ]

testPaginate = test [
               -- empty list
               "empty list paginates to empty list" ~: (U.paginate 20 10 ([]::[Int])) ~?= ([]::[Int])
               -- spot testing
               , "first 10-item page of [1..500]" ~: (U.paginate 10 1 [1..500]) ~?= [1..10]
               , "10th 10-item page of [1..500]" ~: (U.paginate 10 10 [1..500]) ~?= [91..100]
               -- pages hanging over the end
               , "second 3-item page of [1,2,3,4,5]" ~: (U.paginate 3 2 [1,2,3,4,5]) ~?= [4,5]
               , "third 3-item page of [1,2,3,4,5]" ~: (U.paginate 3 3 [1,2,3,4,5]) ~?= [4,5]           
               -- first page not full
               , "first 3-item page of [1,2]" ~: (U.paginate 3 1 [1,2]) ~?= [1,2] ]

testLastPage = test [
               -- empty list
               "last page of empty list is 1" ~: (U.last_page 20 []) ~?= 1
               , -- two pages
               "last 3-item page of [1,2,3,4]" ~: (U.last_page 3 [1,2,3,4]) ~?= 2
               , -- two pages
               "last 3-item page of [1,2,3,4,5,6]" ~: (U.last_page 3 [1,2,3,4,5,6]) ~?= 2
               , -- fifty pages
               "last 10-item page of [1..500]" ~: (U.last_page 10 [1..500]) ~?= 50
               , -- first page not full
               "last 3-item page of [1,2]" ~: (U.last_page 3 [1,2]) ~?= 1 ]

_n = CalendarTime { ctYear = 2000
                  , ctMonth = January
                  , ctDay = 1
                  , ctHour = 5
                  , ctMin = 9
                  , ctSec = 0
                  , ctPicosec = 0
                  , ctTZName = "UTC"
                  , ctTZ = 0
                  , ctWDay = Wednesday
                  , ctYDay = 5
                  , ctIsDST = False}

_m = CalendarTime { ctYear = 2000
                  , ctMonth = October
                  , ctDay = 11
                  , ctHour = 12
                  , ctMin = 55
                  , ctSec = 33
                  , ctPicosec = 0
                  , ctTZName = "UTC"
                  , ctTZ = 0
                  , ctWDay = Sunday
                  , ctYDay = 285
                  , ctIsDST = False}


testTimeFormatting = test [
                     -- check for delimiters
                     "delimiters when padded" ~: "--T::Z" ~=? [f!!4,f!!7,f!!10,f!!13,f!!16,f!!19]
                     , "delimiters when unpadded" ~: "--T::Z" ~=? [g!!4,g!!7,g!!10,g!!13,g!!16,g!!19]
                     -- check lengths
                     , "length when padded" ~: (length f) ~?= 20
                     , "length when unpadded" ~: (length g) ~?= 20
                     -- check components
                     , "year" ~: (take 4 f) ~?= "2000"
                     , "zero-padded month" ~: (take 2 $ drop 5 f) ~?= "01"
                     , "zero-padded day" ~: (take 2 $ drop 8 f) ~?= "01"
                     , "zero-padded hour" ~: (take 2 $ drop 11 f) ~?= "05" 
                     , "zero-padded minutes" ~: (take 2 $ drop 14 f) ~?= "09"
                     , "zero-padded seconds" ~: (take 2 $ drop 17 f) ~?= "00"
                     , "year" ~: (take 4 g) ~?= "2000"
                     , "unpadded month" ~: (take 2 $ drop 5 g) ~?= "10"
                     , "unpadded day" ~: (take 2 $ drop 8 g) ~?= "11"
                     , "unpadded hour" ~: (take 2 $ drop 11 g) ~?= "12" 
                     , "unpadded minutes" ~: (take 2 $ drop 14 g) ~?= "55"
                     , "unpadded seconds" ~: (take 2 $ drop 17 g) ~?= "33"
 ]
    where
      f = U.format_time _n
      g = U.format_time _m
                     

testTimestampTransformations = test [
                                -- check ISO 8601 -> RFC 1123
                                "Conversion of a date/time without padded entries" ~: (U.iso8601toRfc1123 "2008-02-29T21:47:30Z") ~?= "Fri, 29 Feb 2008 21:47:30 GMT"
                               , "Conversion of a date/time with padded entries" ~: (U.iso8601toRfc1123 "2008-02-08T01:07:00Z") ~?= "Fri, 08 Feb 2008 01:07:00 GMT"
                               -- check RFC 1123 -> ISO 8601
                               , "Conversion of RFC 1123 to ISO 8601" ~: (U.httpDateToIso8601 "Thu, 29 Feb 2008 21:47:30 GMT") ~?= "2008-02-29T21:47:30Z"
                               -- check RFC 850 -> ISO 8601
                               , "Conversion of RFC 850 to ISO 8601" ~: (U.httpDateToIso8601 "Thursday, 29-Feb-08 21:47:30 GMT") ~?= "2008-02-29T21:47:30Z"
                               -- check ANSI C -> ISO 8601
                               , "Conversion of ANSI C to ISO 8601" ~: (U.httpDateToIso8601 "Thu Feb 29 21:47:30 2008") ~?= "2008-02-29T21:47:30Z"
                               ]