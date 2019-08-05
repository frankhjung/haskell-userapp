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

import qualified Data.Text    as T (words)
import qualified Data.Text.IO as TIO (getLine)
import           Lib

-- | Usage: echo password | runhaskell fsih0501.hs
-- To run with args:
--
-- > echo username password | runhaskell fsih0701.hs
main :: IO ()
main = T.words <$> TIO.getLine >>=
  \[username, password] -> print $ makeUser (Username username) (Password password)
