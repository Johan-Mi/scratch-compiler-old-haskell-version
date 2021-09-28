{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Macro
  ( expandMacros
  ) where

import Control.Monad (foldM)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Monoid (First(..))
import qualified Data.Text as T
import LispAST (LispAST(..), asTheFunction, getSym, subTrees)
import Text.Printf (printf)
import Utils.Either (maybeToRight)

data MacroError
  = WrongArgCount T.Text Int Int
  | NonSymbolParam T.Text
  | NonSymbolMacroName
  | UnknownMetaVar T.Text T.Text
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

expandMacros :: [LispAST] -> Either MacroError [LispAST]
expandMacros =
  fmap snd .
  foldM
    (\(macros, accum) ast -> do
       ast' <- expand macros ast
       case mkMacro ast' of
         Just m ->
           m <&> \m' -> (getFirst . ((<>) `on` (First .)) macros m', accum)
         Nothing -> return (macros, accum ++ [ast']))
    (getFirst . foldMap (First .) builtinMacros, [])

builtinMacros :: [Macro]
builtinMacros =
  [ \case
      (LispNode (LispSym "sym-concat!") args) ->
        Right . LispSym . T.concat <$> traverse getSym args
      _ -> Nothing
  ]
