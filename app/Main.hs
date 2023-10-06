{-# LANGUAGE OverloadedStrings #-}

{-|
  Module      : UserApp
  Description : Username and password example application.
  Copyright   : © Frank Jung, 2019-2020
  License     : BSD3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable
-}

module Main (main) where

import           Data.Version       (showVersion)
import           Paths_UserApp      (version)
import           System.Environment (getArgs)
import           UserLib            (Password (Password), Username (Username),
                                     display)

import qualified Data.Text          as T (pack)

-- | Display usage information.
usage :: [String]
usage =
  [ "Usage: userapp [username] [password]",
    "Process a username and password.",
    "Version: " ++ showVersion version
  ]

-- | Process a username and password.
go :: String -> String -> IO ()
go u p = display (Username username) (Password password)
  where
    username = T.pack u
    password = T.pack p

-- | Main entry point.
--
-- To run with args:
--
-- @
-- stack exec -- main [username] [password]
-- @
main :: IO ()
main = do
  args <- getArgs
  case args of
    [username, password] -> go username password
    _                    -> putStrLn $ unlines usage
