{-# LANGUAGE OverloadedStrings #-}

module Main (main, tests) where

import           Data.Validation (Validation (..))
import           Test.HUnit      (Counts, Test (TestCase, TestList), runTestTT,
                                  (@=?))
import           UserLib         (Error (Error), Password (Password),
                                  User (User), Username (Username), checkLength,
                                  cleanWhitespace, makeUser, parsePassword,
                                  parseUsername, requireAlphaNum)

tests :: Test
tests = TestList
        [ TestCase (Failure (Error ["Value cannot be empty"]) @=? cleanWhitespace "")
        , TestCase (Failure (Error ["Invalid username:", "Value cannot be empty"]) @=? parseUsername (Username ""))
        , TestCase (Failure (Error ["Invalid password:", "Value cannot be empty"]) @=? parsePassword (Password ""))
        , TestCase (Failure (Error ["Value cannot contain special characters"]) @=? requireAlphaNum "frank12#")
        , TestCase (Failure (Error ["Invalid username:", "Value cannot contain special characters"]) @=? parseUsername (Username "frank12#"))
        , TestCase (Failure (Error ["Invalid password:", "Value cannot contain special characters"]) @=? parsePassword (Password "frank12#"))
        , TestCase (Failure (Error ["Value too long, can not exceed 5"]) @=? checkLength 5 "abcdef")
        , TestCase (Failure (Error ["Invalid username:", "Value too long, can not exceed 10"]) @=? parseUsername (Username "abcdefghjiabc"))
        , TestCase (Failure (Error ["Invalid password:", "Value too long, can not exceed 15"]) @=? parsePassword (Password "abcdefghji123456"))
        , TestCase (Success "frank1234" @=? cleanWhitespace "f r a n k 1 2 3 4 ")
        , TestCase (Success (Username "frank1234") @=? parseUsername (Username "f r a n k 1 2 3 4 "))
        , TestCase (Success (Password "frank1234") @=? parsePassword (Password "f r a n k 1 2 3 4 "))
        , TestCase (Failure (Error ["Invalid username:", "Value cannot contain special characters", "Value too long, can not exceed 10"]) @=? makeUser (Username "@bcdefghjiabc") (Password "abcdef123"))
        , TestCase (Failure (Error ["Invalid password:", "Value cannot contain special characters", "Value too long, can not exceed 15"]) @=? makeUser (Username "frank") (Password "@bcdefghji123456"))
        , TestCase (Success (User (Username "frank") (Password "abcdef123")) @=? makeUser (Username "frank") (Password "abcdef123"))
        ]

main :: IO Counts
main = runTestTT tests
