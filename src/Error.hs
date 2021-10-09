{-# LANGUAGE ExistentialQuantification #-}

module Error
  ( IsError
  , Error(..)
  ) where

import Text.Parsec (ParseError)

class Show e =>
      IsError e


data Error =
  forall e. IsError e =>
            Error e

instance Show Error where
  show (Error e) = show e

instance IsError ParseError
