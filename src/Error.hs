{-# LANGUAGE ExistentialQuantification #-}

module Error
  ( IsError(..)
  , Error(..)
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Text.Parsec (ParseError)

class IsError e where
  showError :: e -> T.Text
  printError :: e -> IO ()
  printError = IO.putStrLn . showError

data Error =
  forall e. IsError e =>
            Error e

instance IsError Error where
  showError (Error e) = showError e

instance IsError ParseError where
  showError = T.pack . show
