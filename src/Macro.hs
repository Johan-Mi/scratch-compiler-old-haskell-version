{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Macro
  ( expandMacros
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Monad (foldM)
import Control.Monad.Except (ExceptT(..), liftEither, throwError)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..))
import qualified Data.Text as T
import Error (Error(..), IsError)
import LispAST (LispAST(..), asTheFunction, getSym, subTrees)
import Parser (programP)
import Text.Parsec.Text (parseFromFile)
import Text.Printf (printf)
import Utils.Either (maybeToRight)

data MacroError
  = WrongArgCount T.Text Int Int
  | NonSymbolParam T.Text
  | NonSymbolMacroName
  | UnknownMetaVar T.Text T.Text
  | InvalidArgsForInclude
  | InvalidMacroDefinition -- Very unspecific, TODO: Better errors
  deriving (Eq)

instance Show MacroError where
  show (WrongArgCount name expected got) =
    printf
      "function macro `%s` expected %d arguments but got %d"
      name
      expected
      got
  show (NonSymbolParam name) =
    printf "non-symbol in parameter list of function macro `%s`" name
  show NonSymbolMacroName = "non-symbol used as macro name"
  show (UnknownMetaVar name var) =
    printf "unknown meta-variable `%s` in body of function macro `%s`" var name
  show InvalidMacroDefinition = "invalid macro definition"
  show InvalidArgsForInclude = "invalid args for an `include` statement"

instance IsError MacroError

type Macro = LispAST -> Maybe (Either MacroError LispAST)

mkMacro :: LispAST -> Maybe (Either MacroError Macro)
mkMacro =
  flip asTheFunction "macro" $ \case
    [LispSym name, body] ->
      Right $ \case
        LispSym name'
          | name' == name -> Just $ Right body
        _ -> Nothing
    [LispNode name params, body] -> do
      name' <- maybeToRight NonSymbolMacroName $ getSym name
      params' <- maybeToRight (NonSymbolParam name') $ traverse getSym params
      return $ mkFuncMacro name' params' body
    _ -> Left InvalidMacroDefinition

mkFuncMacro :: T.Text -> [T.Text] -> LispAST -> Macro
mkFuncMacro name params body = f `asTheFunction` name
  where
    f args
      | nargs /= nparams = Left $ WrongArgCount name nparams nargs
      | otherwise = subst name (zip params args) body
      where
        nargs = length args
        nparams = length params

subst :: T.Text -> [(T.Text, LispAST)] -> LispAST -> Either MacroError LispAST
subst name mvars = go
  where
    go (LispUnquote (LispSym sym)) =
      maybeToRight (UnknownMetaVar name sym) $ lookup sym mvars
    go (LispUnquote ast) = pure ast
    go ast = subTrees go ast

expand :: Macro -> LispAST -> Either MacroError LispAST
expand m = go
  where
    go ast = subTrees go ast >>= \ast' -> maybe (pure ast') (>>= go) $ m ast'

include :: LispAST -> Maybe (ExceptT Error IO [LispAST])
include =
  flip asTheFunction "include" $ \case
    [LispString path] ->
      ExceptT $ left Error <$> parseFromFile programP (T.unpack path)
    _ -> throwError $ Error InvalidArgsForInclude

expandList ::
     (Macro, [LispAST]) -> [LispAST] -> ExceptT Error IO (Macro, [LispAST])
expandList =
  foldM $ \(macros, accum) ast -> do
    ast' <- liftEither $ left Error $ expand macros ast
    let theMacro =
          mkMacro ast' <&> liftEither . left Error .
          fmap (\m' -> (getFirst . (First . macros <> First . m'), accum))
    let theInclude = include ast <&> (>>= expandList (macros, accum))
    let theNormal = return (macros, accum ++ [ast'])
    fromMaybe theNormal $ theMacro <|> theInclude

expandMacros :: [LispAST] -> ExceptT Error IO [LispAST]
expandMacros =
  fmap snd . expandList (getFirst . foldMap (First .) builtinMacros, [])

builtinMacros :: [Macro]
builtinMacros =
  [ \case
      (LispNode (LispSym "sym-concat!") args) ->
        Right . LispSym . T.concat <$> traverse getSym args
      _ -> Nothing
  ]
