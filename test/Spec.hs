module Main (main) where

import           Lib
import           Test.HUnit

main :: IO Counts
main = runTestTT $
        TestList
          [ TestCase (Left (Error "Value cannot be empty") @=? cleanWhitespace "")
          , TestCase (Left (Error "Value cannot be empty") @=? validateUsername (Username ""))
          , TestCase (Left (Error "Value cannot be empty") @=? validatePassword (Password ""))
          , TestCase (Left (Error "Value cannot contain special characters") @=? requireAlphaNum "frank12#")
          , TestCase (Left (Error "Value cannot contain special characters") @=? validateUsername (Username "frank12#"))
          , TestCase (Left (Error "Value cannot contain special characters") @=? validatePassword (Password "frank12#"))
          , TestCase (Left (Error "Value too long, can not exceed 5") @=? checkLength 5 "abcdef")
          , TestCase (Left (Error "Value too long, can not exceed 10") @=? validateUsername (Username "abcdefghjiabc"))
          , TestCase (Left (Error "Value too long, can not exceed 20") @=? validatePassword (Password "abcdefghji1234567890abc"))
          , TestCase (Right "frank1234" @=? cleanWhitespace "f r a n k 1 2 3 4 ")
          , TestCase (Right (Username "frank1234") @=? validateUsername (Username "f r a n k 1 2 3 4 "))
          , TestCase (Right (Password "frank1234") @=? validatePassword (Password "f r a n k 1 2 3 4 "))
          , TestCase (Right (User (Username "frank") (Password "abcdef123")) @=? makeUser (Username "frank") (Password "abcdef123"))
          ]

