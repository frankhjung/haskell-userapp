{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Validation (Validation (..))
import           Lib
import           Test.HUnit

main :: IO Counts
main = runTestTT $
        TestList
          [ TestCase (Failure (Error ["Value cannot be empty"]) @=? cleanWhitespace "")
          , TestCase (Failure (Error ["Value cannot be empty"]) @=? validateUsername (Username ""))
          , TestCase (Failure (Error ["Value cannot be empty"]) @=? validatePassword (Password ""))
          , TestCase (Failure (Error ["Value cannot contain special characters"]) @=? requireAlphaNum "frank12#")
          , TestCase (Failure (Error ["Value cannot contain special characters"]) @=? validateUsername (Username "frank12#"))
          , TestCase (Failure (Error ["Value cannot contain special characters"]) @=? validatePassword (Password "frank12#"))
          , TestCase (Failure (Error ["Value too long, can not exceed 5"]) @=? checkLength 5 "abcdef")
          , TestCase (Failure (Error ["Value too long, can not exceed 10"]) @=? validateUsername (Username "abcdefghjiabc"))
          , TestCase (Failure (Error ["Value too long, can not exceed 15"]) @=? validatePassword (Password "abcdefghji123456"))
          , TestCase (Success "frank1234" @=? cleanWhitespace "f r a n k 1 2 3 4 ")
          , TestCase (Success (Username "frank1234") @=? validateUsername (Username "f r a n k 1 2 3 4 "))
          , TestCase (Success (Password "frank1234") @=? validatePassword (Password "f r a n k 1 2 3 4 "))
          , TestCase (Failure (Error ["Value cannot contain special characters", "Value too long, can not exceed 10"]) @=? makeUser (Username "@bcdefghjiabc") (Password "abcdef123"))
          , TestCase (Failure (Error ["Value cannot contain special characters", "Value too long, can not exceed 15"]) @=? makeUser (Username "frank") (Password "@bcdefghji123456"))
          , TestCase (Success (User (Username "frank") (Password "abcdef123")) @=? makeUser (Username "frank") (Password "abcdef123"))
          ]

