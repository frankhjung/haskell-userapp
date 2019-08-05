{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Validation (Validation (..))
import           Lib
import           Test.HUnit

main :: IO Counts
main = runTestTT $
        TestList
          [ TestCase (Failure (errorMessage "Value cannot be empty") @=? cleanWhitespace "")
          , TestCase (Failure (errorMessage "Invalid username:" <> errorMessage "Value cannot be empty") @=? parseUsername (Username ""))
          , TestCase (Failure (errorMessage "Invalid password:" <> errorMessage "Value cannot be empty") @=? parsePassword (Password ""))
          , TestCase (Failure (errorMessage "Value cannot contain special characters") @=? requireAlphaNum "frank12#")
          , TestCase (Failure (errorMessage "Invalid username:" <> errorMessage "Value cannot contain special characters") @=? parseUsername (Username "frank12#"))
          , TestCase (Failure (errorMessage "Invalid password:" <> errorMessage "Value cannot contain special characters") @=? parsePassword (Password "frank12#"))
          , TestCase (Failure (errorMessage "Value too long, can not exceed 5") @=? checkLength 5 "abcdef")
          , TestCase (Failure (errorMessage "Invalid username:" <> errorMessage "Value too long, can not exceed 10") @=? parseUsername (Username "abcdefghjiabc"))
          , TestCase (Failure (errorMessage "Invalid password:" <> errorMessage "Value too long, can not exceed 15") @=? parsePassword (Password "abcdefghji123456"))
          , TestCase (Success "frank1234" @=? cleanWhitespace "f r a n k 1 2 3 4 ")
          , TestCase (Success (Username "frank1234") @=? parseUsername (Username "f r a n k 1 2 3 4 "))
          , TestCase (Success (Password "frank1234") @=? parsePassword (Password "f r a n k 1 2 3 4 "))
          , TestCase (Failure (errorMessage "Invalid username:" <> errorMessage "Value cannot contain special characters" <> errorMessage "Value too long, can not exceed 10") @=? makeUser (Username "@bcdefghjiabc") (Password "abcdef123"))
          , TestCase (Failure (errorMessage "Invalid password:" <> errorMessage "Value cannot contain special characters" <> errorMessage "Value too long, can not exceed 15") @=? makeUser (Username "frank") (Password "@bcdefghji123456"))
          , TestCase (Success (User (Username "frank") (Password "abcdef123")) @=? makeUser (Username "frank") (Password "abcdef123"))
          ]

