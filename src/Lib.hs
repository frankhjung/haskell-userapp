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

import           Data.Char (isAlphaNum)

newtype Username = Username String deriving (Eq, Show)
newtype Password = Password String deriving (Eq, Show)
newtype Error = Error String deriving (Eq, Show)

data User = User Username Password deriving (Eq, Show)

makeUser :: Username -> Password -> Either Error User
makeUser username password =
    User <$> validateUsername username <*> validatePassword password

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (Error "Value cannot be empty")
cleanWhitespace xs = Right (filter (/= ' ') xs)

requireAlphaNum :: String -> Either Error String
requireAlphaNum xs
  | all isAlphaNum xs = Right xs
  | otherwise = Left (Error "Value cannot contain special characters")

checkLength :: Int -> String -> Either Error String
checkLength maxlength input
  | length input <= maxlength = Right input
  | otherwise = Left (Error ("Value too long, can not exceed " ++ show maxlength))

validatePassword :: Password -> Either Error Password
validatePassword (Password password) =
  cleanWhitespace password >>=
  requireAlphaNum >>=
  checkLength 20 >>=
  \p -> Right (Password p)

validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
  cleanWhitespace username >>=
  requireAlphaNum >>=
  checkLength 10 >>=
  \u -> Right (Username u)

