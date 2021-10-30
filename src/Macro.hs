{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Macro
  ( expandMacros
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Monad (foldM, guard)
import Control.Monad.Except (ExceptT(..), liftEither, throwError)
import Data.Either (isRight)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..))
import qualified Data.Text as T
import Error (Error(..), IsError)
import LispAST (LispAST(..), asTheFunction, getStr, getSym, subTrees)
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

type MacroM = ExceptT Error IO

type Macro = LispAST -> Maybe (MacroM LispAST)

mkMacro :: LispAST -> Maybe (Either MacroError Macro)
mkMacro =
  flip asTheFunction "macro" $ \case
    [LispSym name, body] ->
      Right $ \case
        LispSym name'
          | name' == name -> Just $ pure body
        _ -> Nothing
    [LispNode name params, body] -> do
      name' <- maybeToRight NonSymbolMacroName $ getSym name
      params' <- maybeToRight (NonSymbolParam name') $ traverse getSym params
      pure $ mkFuncMacro name' params' body
    _ -> Left InvalidMacroDefinition

mkFuncMacro :: T.Text -> [T.Text] -> LispAST -> Macro
mkFuncMacro name params body = f `asTheFunction` name
  where
    f args
      | nargs /= nparams = throwError $ Error $ WrongArgCount name nparams nargs
      | otherwise = subst name (zip params args) body
      where
        nargs = length args
        nparams = length params

subst :: T.Text -> [(T.Text, LispAST)] -> LispAST -> MacroM LispAST
subst name mvars = go
  where
    go :: LispAST -> MacroM LispAST
    go (LispUnquote (LispSym sym)) =
      maybe (throwError $ Error $ UnknownMetaVar name sym) pure $
      lookup sym mvars
    go (LispUnquote ast) = pure ast
    go ast = subTrees go ast

expand :: Macro -> LispAST -> MacroM LispAST
expand m = go
  where
    go ast = subTrees go ast >>= \ast' -> maybe (pure ast') (>>= go) $ m ast'

include :: LispAST -> Maybe (MacroM [LispAST])
include =
  flip asTheFunction "include" $ \case
    [LispString path] ->
      ExceptT $ left Error <$> parseFromFile programP (T.unpack path)
    _ -> throwError $ Error InvalidArgsForInclude

expandList :: (Macro, [LispAST]) -> [LispAST] -> MacroM (Macro, [LispAST])
expandList =
  foldM $ \(macros, accum) ast -> do
    ast' <- expand macros ast
    let theMacro =
          mkMacro ast' <&> liftEither . left Error .
          fmap (\m' -> (getFirst . (First . macros <> First . m'), accum))
    let theInclude = include ast <&> (>>= expandList (macros, accum))
    let theNormal = pure (macros, accum ++ [ast'])
    fromMaybe theNormal $ theMacro <|> theInclude

expandMacros :: [LispAST] -> MacroM [LispAST]
expandMacros =
  fmap snd . expandList (getFirst . foldMap (First .) builtinMacros, [])

builtinMacros :: [Macro]
builtinMacros =
  [ \case
      (LispNode (LispSym "sym-concat!") args) ->
        pure . LispSym . T.concat <$> traverse getSym args
      _ -> Nothing
  , \case
      (LispNode (LispSym "str-concat!") args) ->
        pure . LispString . T.concat <$> traverse getStr args
      _ -> Nothing
  , \case
      (LispNode fn asts) -> do
        let maybeIncludes = maybeToRight . pure . pure <*> include <$> asts
        guard $ any isRight maybeIncludes
        let asts' = either id id <$> maybeIncludes
        pure $ LispNode fn . concat <$> sequenceA asts'
      _ -> Nothing
  ]
