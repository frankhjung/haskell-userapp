{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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
        , checkPasswordLength
        , checkUsernameLength
        , cleanWhitespace
        , display
        , parsePassword
        , parseUsername
        , requireAlphaNum
        , validatePassword
        , validateUsername
        ) where

import           Data.Char       (isAlphaNum)
import           Data.Coerce
import           Data.Text       (Text)
import qualified Data.Text       as T (all, concat, filter, length, pack, strip,
                                       unlines)
import qualified Data.Text.IO    as TIO (putStr, putStrLn)
import           Data.Validation (Validation (..))

newtype Error = Error [Text] deriving (Eq, Show, Semigroup)
newtype Username = Username Text deriving (Eq, Show)
newtype Password = Password Text deriving (Eq, Show)
data User = User Username Password deriving (Eq, Show)
type Rule a = (a -> Validation Error a)

makeUser :: Username -> Password -> Validation Error User
makeUser username password =
  User <$> parseUsername username <*> parsePassword password

cleanWhitespace :: Rule Text
cleanWhitespace "" = Failure (Error ["Value cannot be empty"])
cleanWhitespace as = Success (T.filter (/= ' ') (T.strip as))

checkLength :: Int -> Rule Text
checkLength maxlength as =
  if T.length as <= maxlength
    then Success as
    else Failure (Error ["Value too long, can not exceed " <> (T.pack . show) maxlength])

checkPasswordLength :: Rule Password
checkPasswordLength = coerce (checkLength 15) :: Rule Password

checkUsernameLength :: Rule Username
checkUsernameLength = coerce (checkLength 10) :: Rule Username

display :: Username -> Password -> IO ()
display username password =
  case makeUser username password of
    Success (User u _) -> TIO.putStrLn (T.concat ["Welcome ", coerce u])
    Failure (Error e)  -> TIO.putStr (T.unlines e)

parsePassword :: Rule Password
parsePassword password =
  case validatePassword password of
    Success p -> Success p
    Failure e -> Failure (Error ["Invalid password:"] <> e)

parseUsername :: Rule Username
parseUsername username =
  case validateUsername username of
    Success u -> Success u
    Failure e -> Failure (Error ["Invalid username:"] <> e)

requireAlphaNum :: Rule Text
requireAlphaNum as =
  if T.all isAlphaNum as
    then Success as
    else Failure (Error ["Value cannot contain special characters"])

validatePassword :: Rule Password
validatePassword password =
  case (coerce cleanWhitespace :: Rule Password) password of
    Success p -> (coerce requireAlphaNum :: Rule Password) p *> checkPasswordLength p *> Success p
    Failure e -> Failure e

validateUsername :: Rule Username
validateUsername username =
  case (coerce cleanWhitespace :: Rule Username) username of
    Success u -> (coerce requireAlphaNum :: Rule Username) u *> checkUsernameLength u *> Success u
    Failure e -> Failure e
