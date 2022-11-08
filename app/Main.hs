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

import qualified Data.Text    as T (words)
import qualified Data.Text.IO as TIO (getLine)

import           UserLib      (Password (Password), Username (Username),
                               display)

-- To run with args:
--
-- > echo username password | stack exec -- userapp-exe
main :: IO ()
main =
  TIO.getLine >>=
    ( \[username, password] -> display (Username username) (Password password) ) . T.words
