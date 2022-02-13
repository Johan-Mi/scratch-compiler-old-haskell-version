{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Macro
  ( expandMacros
  ) where

import Control.Arrow (left)
import Control.Monad ((>=>))
import Control.Monad.Except (Except, ExceptT(..), throwError, withExcept)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Strict (WriterT, execWriterT, tell)
import Data.Foldable (traverse_)
import Data.Monoid (Any(..), First(..))
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.Traversable (for)
import Error (Error(..), IsError(..))
import LispAST (LispAST(..), asTheFunction, getStr, getSym, subTrees)
import Parser (programP)
import Text.Parsec.Text (parseFromFile)
import Utils.Trans (hoistExcept, orThrow)

data MacroError
  = WrongArgCount T.Text Int Int
  | NonSymbolParam T.Text
  | NonSymbolMacroName
  | UnknownMetaVar T.Text T.Text
  | InvalidArgsForInclude
  | InvalidMacroDefinition -- Very unspecific, TODO: Better errors

instance IsError MacroError where
  showError (WrongArgCount name expected got) =
    "function macro `" <>
    name <>
    "` expected " <>
    T.pack (show expected) <> " arguments but got " <> T.pack (show got)
  showError (NonSymbolParam name) =
    "non-symbol in parameter list of function macro `" <> name <> "`"
  showError NonSymbolMacroName = "non-symbol used as macro name"
  showError (UnknownMetaVar name var) =
    "unknown meta-variable `" <>
    var <> "` in body of function macro `" <> name <> "`"
  showError InvalidMacroDefinition = "invalid macro definition"
  showError InvalidArgsForInclude = "invalid args for an `include` statement"

type MacroM = ExceptT Error IO

type Macro = LispAST -> Maybe (MacroM LispAST)

mkMacro :: [LispAST] -> Except MacroError Macro
mkMacro [LispSym name, body] =
  pure $ \case
    LispSym name'
      | name' == name -> Just $ pure body
    _ -> Nothing
mkMacro [LispNode name params, body] = do
  name' <- orThrow NonSymbolMacroName $ getSym name
  params' <- orThrow (NonSymbolParam name') $ traverse getSym params
  pure $ mkFuncMacro name' params' body
mkMacro _ = throwError InvalidMacroDefinition

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
      orThrow (Error $ UnknownMetaVar name sym) $ lookup sym mvars
    go (LispUnquote ast) = pure ast
    go ast = subTrees go ast

expand :: Macro -> LispAST -> MacroM LispAST
expand m = go
  where
    go = subTrees go >=> \ast -> maybe (pure ast) (>>= go) $ m ast

include :: [LispAST] -> MacroM [LispAST]
include [LispString path] =
  ExceptT $ left Error <$> parseFromFile programP (T.unpack path)
include _ = throwError $ Error InvalidArgsForInclude

expandList :: [LispAST] -> WriterT [LispAST] (StateT Macro MacroM) ()
expandList =
  traverse_ $ \ast -> do
    macros <- get
    ast' <- lift $ lift $ expand macros ast
    case ast' of
      (LispNode (LispSym "macro") args) -> do
        m <- lift $ lift $ hoistExcept $ withExcept Error $ mkMacro args
        put $ getFirst . (First . macros <> First . m)
      (LispNode (LispSym "include") args) ->
        lift (lift $ include args) >>= expandList
      other -> tell [other]

expandMacros :: [LispAST] -> MacroM [LispAST]
expandMacros =
  flip evalStateT (getFirst . foldMap (First .) builtinMacros) .
  execWriterT . expandList

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
      (LispNode fn asts)
        | didSomething -> Just $ LispNode fn . concat <$> sequenceA asts'
        where (Any didSomething, asts') =
                for asts $ \case
                  (LispNode (LispSym "include") args) ->
                    (Any True, include args)
                  ast -> (Any False, pure [ast])
      _ -> Nothing
  , \case
      (LispNode (LispSym "include-str") [LispString path]) ->
        Just $ lift $ LispString <$> IO.readFile (T.unpack path)
      _ -> Nothing
  ]
