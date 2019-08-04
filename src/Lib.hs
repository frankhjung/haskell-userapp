{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
        (
          -- * Data Types
          User (..)
        , Username (..)
        , Password (..)
        , Error (..)
          -- * Constructors
        , makeUser
          -- * Functions
        , cleanWhitespace
        , requireAlphaNum
        , checkLength
        , validatePassword
        , validateUsername
        ) where

-- Find Success in Haskell
-- Chapter 7 - applicative username & password
--
-- Installed HUnit-1.6.0.0 with Cabal

import           Data.Char       (isAlphaNum)
import           Data.Validation (Validation (..))

newtype Username = Username String deriving (Eq, Show)
newtype Password = Password String deriving (Eq, Show)
newtype Error = Error [String] deriving (Eq, Show, Semigroup)

-- instance Semigroup Error where
--  Error xs <> Error ys = Error (xs <> ys)

data User = User Username Password deriving (Eq, Show)

makeUser :: Username -> Password -> Validation Error User
makeUser username password =
  User <$> validateUsername username <*> validatePassword password

cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (Error ["Value cannot be empty"])
cleanWhitespace xs = Success (filter (/= ' ') xs)

requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs
  | all isAlphaNum xs = Success xs
  | otherwise = Failure (Error ["Value cannot contain special characters"])

checkLength :: Int -> String -> Validation Error String
checkLength maxlength input
  | length input <= maxlength = Success input
  | otherwise = Failure (Error ["Value too long, can not exceed " ++ show maxlength])

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
  case cleanWhitespace password of
    Failure err -> Failure err
    Success p   -> requireAlphaNum p *> checkLength 15 p *> Success (Password p)

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  case cleanWhitespace username of
    Failure err -> Failure err
    Success u   -> requireAlphaNum u *> checkLength 10 u *> Success (Username u)

