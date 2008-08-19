module Main where

import Test.HUnit
import Blog.Model.EntryTests
import UtilitiesTests

main :: IO Counts
main = runTestTT allTests

allTests = TestList [
           -- tests Utilities
           testUtilities
           -- tests Blog.Model.Entry
           , testBlogModel 
           ]