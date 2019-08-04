{-|
  Module      : Lib
  Description : Supporting functions for user password example application
  Copyright   : © Frank Jung, 2019
  License     : BSD3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable
-}

module Main where

import           Lib

-- | Usage: echo password | runhaskell fsih0501.hs
-- To run with args:
--
-- > echo username password | runhaskell fsih0701.hs
main :: IO ()
main = words <$> getLine >>=
  \[username, password] -> print $ makeUser (Username username) (Password password)
