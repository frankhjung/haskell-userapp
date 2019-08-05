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
        , cleanWhitespace
        , display
        , parsePassword
        , parseUsername
        , requireAlphaNum
        , validatePassword
        , validateUsername
        ) where

import           Data.Char       (isAlphaNum)
-- import           Data.Coerce
import           Data.Text       (Text)
import qualified Data.Text       as T (all, concat, filter, length, pack, strip,
                                       unlines)
import qualified Data.Text.IO    as TIO (putStr, putStrLn)
import           Data.Validation (Validation (..))

-- instance Semigroup Error where
--  Error xs <> Error ys = Error (xs <> ys)

newtype Error = Error [Text] deriving (Eq, Show, Semigroup)
newtype Username = Username Text deriving (Eq, Show)
newtype Password = Password Text deriving (Eq, Show)
data User = User Username Password deriving (Eq, Show)

makeUser :: Username -> Password -> Validation Error User
makeUser username password =
  User <$> parseUsername username <*> parsePassword password

cleanWhitespace :: Text -> Validation Error Text
cleanWhitespace "" = Failure (Error ["Value cannot be empty"])
cleanWhitespace xs = Success (T.filter (/= ' ') (T.strip xs))

checkLength :: Int -> Text -> Validation Error Text
checkLength maxlength input =
  if T.length input <= maxlength
    then Success input
    else Failure (Error ["Value too long, can not exceed " <> (T.pack . show) maxlength])

display :: Username -> Password -> IO ()
display username password =
  case makeUser username password of
    Success (User (Username u) _) -> TIO.putStrLn (T.concat ["Welcome ", u])
    Failure (Error errors)        -> TIO.putStr (T.unlines errors)

parsePassword :: Password -> Validation Error Password
parsePassword password =
  case validatePassword password of
    Success p -> Success p
    Failure e -> Failure (Error ["Invalid password:"] <> e)

parseUsername :: Username -> Validation Error Username
parseUsername username =
  case validateUsername username of
    Success u -> Success u
    Failure e -> Failure (Error ["Invalid username:"] <> e)

requireAlphaNum :: Text -> Validation Error Text
requireAlphaNum xs =
  if T.all isAlphaNum xs
    then Success xs
    else Failure (Error ["Value cannot contain special characters"])

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
  case cleanWhitespace password of
    Success p -> requireAlphaNum p *> checkLength maxlength p *> Success (Password p)
    Failure e -> Failure e
  where maxlength = 15 :: Int

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  case cleanWhitespace username of
    Success u -> requireAlphaNum u *> checkLength maxlength u *> Success (Username u)
    Failure e -> Failure e
  where maxlength = 10 :: Int
