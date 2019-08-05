{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

{-|
  Module      : Lib
  Description : Supporting functions for user password example application
  Copyright   : © Frank Jung, 2019
  License     : BSD3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable
-}

module Lib
        (
          -- * Data Types
          Error (..)
        , Password (..)
        , User (..)
        , Username (..)
          -- * Constructors
        , makeUser
          -- * Functions
        , checkLength
        , cleanWhitespace
        , display
        , errorMessage
        , parsePassword
        , parseUsername
        , requireAlphaNum
        , validatePassword
        , validateUsername
        ) where

import           Data.Char          (isAlphaNum)
import           Data.Coerce
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE (toList)
import           Data.Validation    (Validation (..))

-- instance Semigroup Error where
--  Error xs <> Error ys = Error (xs <> ys)

-- | Error as non-empty string.
newtype Error = Error (NonEmpty String) deriving (Eq, Show, Semigroup)
-- | The user name.
newtype Username = Username String deriving (Eq, Show)
-- | The password.
newtype Password = Password String deriving (Eq, Show)
-- | The user as name and password.
data User = User Username Password deriving (Eq, Show)

-- | Make a user given a user name and password.
makeUser :: Username -> Password -> Validation Error User
makeUser username password =
  User <$> parseUsername username <*> parsePassword password

-- | Remove whitespace from a string.
cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (errorMessage "Value cannot be empty")
cleanWhitespace xs = Success (filter (/= ' ') xs)

-- | Ensure string does not exceed given length.
checkLength :: Int -> String -> Validation Error String
checkLength maxlength input =
  if length input <= maxlength
    then Success input
    else Failure (errorMessage ("Value too long, can not exceed " ++ show maxlength))

-- | Construct and then display user.
display :: Username -> Password -> IO ()
display username password =
  case makeUser username password of
    Success (User u _) -> putStrLn ("Welcome " ++ coerce u)
    -- Success (User (Username u) _) -> putStrLn ("Welcome " ++ u)
    Failure (Error es) -> putStr (unlines (NE.toList (coerce es)))

-- | Non-empty constructor for errors.
errorMessage :: String -> Error
errorMessage msg = Error (msg :| [])

-- | Make a password.
parsePassword :: Password -> Validation Error Password
parsePassword password =
  case validatePassword password of
    Success p -> Success p
    Failure e -> Failure (errorMessage "Invalid password:" <> e)

-- | Make a user name.
parseUsername :: Username -> Validation Error Username
parseUsername username =
  case validateUsername username of
    Success u -> Success u
    Failure e -> Failure (errorMessage "Invalid username:" <> e)

-- | Check string only contains alpha-numeric characters.
requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
  if all isAlphaNum xs
    then Success xs
    else Failure (errorMessage "Value cannot contain special characters")

-- | Validate a password.
validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
  case cleanWhitespace password of
    Success p -> requireAlphaNum p *> checkLength maxlength p *> Success (Password p)
    Failure e -> Failure e
  where maxlength = 15 :: Int

-- | Validate a user name.
validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  case cleanWhitespace username of
    Success u -> requireAlphaNum u *> checkLength maxlength u *> Success (Username u)
    Failure e -> Failure e
  where maxlength = 10 :: Int
