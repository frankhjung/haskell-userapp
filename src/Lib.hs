{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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
import           Data.Text       (Text)
import qualified Data.Text       as T (all, filter, length, pack, strip)
import           Data.Validation (Validation (..))

newtype Username = Username Text deriving (Eq, Show)
newtype Password = Password Text deriving (Eq, Show)
newtype Error = Error [Text] deriving (Eq, Show, Semigroup)

-- instance Semigroup Error where
--  Error xs <> Error ys = Error (xs <> ys)

data User = User Username Password deriving (Eq, Show)

makeUser :: Username -> Password -> Validation Error User
makeUser username password =
  User <$> validateUsername username <*> validatePassword password

cleanWhitespace :: Text -> Validation Error Text
cleanWhitespace "" = Failure (Error ["Value cannot be empty"])
cleanWhitespace xs = Success (T.filter (/= ' ') (T.strip xs))

requireAlphaNum :: Text -> Validation Error Text
requireAlphaNum xs
  | T.all isAlphaNum xs = Success xs
  | otherwise = Failure (Error ["Value cannot contain special characters"])

checkLength :: Int -> Text -> Validation Error Text
checkLength maxlength input
  | T.length input <= maxlength = Success input
  | otherwise = Failure (Error ["Value too long, can not exceed " <> (T.pack . show) maxlength])

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

