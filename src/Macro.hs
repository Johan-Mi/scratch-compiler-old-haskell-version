{-# LANGUAGE OverloadedStrings #-}

module Macro
  ( expandMacros
  ) where

-- TODO: Support nested macros
import Data.List (partition)
import qualified Data.Text as T
import LispAST (LispAST(..), getSym, isTheFunction)
import Text.Printf (printf)
import Utils.Either (maybeToRight)

type Macro = LispAST -> Either MacroError LispAST

data MacroError
  = FunctionMacroUsedAsSymbol T.Text
  | WrongArgCount T.Text Int Int
  | NonSymbolParam T.Text
  | UnknownMetaVar T.Text T.Text
  | UnquoteNonSymbol T.Text
  deriving (Eq)

instance Show MacroError where
  show (FunctionMacroUsedAsSymbol name) =
    printf "`%s` is a function macro but was used as a symbol" name
  show (WrongArgCount name expected got) =
    printf
      "function macro `%s` expected %d arguments but got %d"
      name
      expected
      got
  show (NonSymbolParam name) =
    printf "non-symbol in parameter list of function macro %s" name
  show (UnknownMetaVar name var) =
    printf "unknown meta-variable `%s` in body of function macro %s" name var
  show (UnquoteNonSymbol name) =
    printf "tried to unquote non-symbol in function macro %s" name

isMacro :: LispAST -> Bool
isMacro = isTheFunction "macro"

mkFuncMacro :: T.Text -> [T.Text] -> LispAST -> Macro
mkFuncMacro name params body = f
  where
    f (LispSym _) = Left $ FunctionMacroUsedAsSymbol name
    f (LispNode _ args)
      | nargs /= nparams = Left $ WrongArgCount name nparams nargs
      | otherwise = subst name (zip params args) body
      where
        nargs = length args
        nparams = length params

mkMacro :: LispAST -> Either MacroError (T.Text, Macro)
mkMacro (LispNode _ [LispSym name, body]) = Right (name, const $ Right body)
mkMacro (LispNode _ [LispNode (LispSym name) astParams, body])
  | Just params <- traverse getSym astParams =
    Right (name, mkFuncMacro name params body)
  | otherwise = Left $ NonSymbolParam name

subst :: T.Text -> [(T.Text, LispAST)] -> LispAST -> Either MacroError LispAST
subst name mvars (LispUnquote (LispSym sym)) =
  maybeToRight (UnknownMetaVar name sym) $ sym `lookup` mvars
subst name mvars (LispNode fun args) =
  LispNode <$> subst name mvars fun <*> traverse (subst name mvars) args
subst name mvars ast = Right ast

withMacros :: [(T.Text, Macro)] -> LispAST -> Either MacroError LispAST
withMacros ms (LispUnquote _) = error "TODO" -- How should this work?
withMacros ms sym@(LispSym str) = maybe (Right sym) ($ sym) $ str `lookup` ms
withMacros ms (LispNode fun args) = do
  fun' <-
    case withMacros ms fun of
      Left (FunctionMacroUsedAsSymbol _) -> Right fun
      f -> f
  args' <- traverse (withMacros ms) args
  let node' = LispNode fun' args'
  case fun' of
    LispSym str -> maybe (Right node') ($ node') $ str `lookup` ms
    _ -> Right node'
withMacros ms ast = Right ast

expandMacros :: [LispAST] -> Either MacroError [LispAST]
expandMacros ast = do
  let (macroDefs, exprs) = partition isMacro ast
  macros <- traverse mkMacro macroDefs
  traverse (withMacros macros) exprs
