{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Block
  ( Env(..)
  , BlockError
  , procToBlocks
  ) where

import Control.Monad (unless, zipWithM)
import Control.Monad.Except (Except, throwError)
import Control.Monad.RWS (RWST, evalRWST)
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.Writer (tell)
import qualified Data.Text as T
import Data.Traversable (for)
import JSON (JValue(..))
import Lens.Micro (Lens', (^.), set)
import Mid.Expr (Expr(..), Value(..))
import Mid.Proc (Procedure(..), Statement(..), procedureName, procedureParams)
import Text.Printf (printf)
import UID (UID, UIDState, idJSON, newID, prependID)

data BlockError
  = InvalidParamsForSpecialProcDef T.Text
  | UnknownProc T.Text
  | InvalidArgsForBuiltinProc T.Text
  | FuncWrongArgCount T.Text ArgCount Int
  | UnknownSymbolInExpr T.Text
  | NonSymbolInProcDef T.Text

instance Show BlockError where
  show (InvalidParamsForSpecialProcDef procName) =
    printf "invalid arguments for definition of special procedure `%s`" procName
  show (UnknownProc procName) = printf "unknown procedure `%s`" procName
  show (InvalidArgsForBuiltinProc procName) =
    printf "invalid arguments for call to builtin procedure `%s`" procName
  show (FuncWrongArgCount name expected got) =
    printf
      "function `%s` expected %s arguments but got %d"
      name
      (show expected)
      got
  show (UnknownSymbolInExpr name) =
    printf "unknown symbol `%s` used in an expression" name
  show (NonSymbolInProcDef name) =
    printf "non-symbol in definition of procedure `%s`" name

data ArgCount
  = Exactly Int
  | AtLeast Int

instance Show ArgCount where
  show (Exactly num) = show num
  show (AtLeast num) = "at least " ++ show num

data Env =
  Env
    { _envParent :: Maybe UID
    , _envNext :: Maybe UID
    , _envProcs :: [T.Text]
    , _envProcArgs :: [T.Text]
    , _envLocalVars :: [(T.Text, UID)]
    , _envGlobalVars :: [(T.Text, UID)]
    , _envLocalLists :: [(T.Text, UID)]
    , _envGlobalLists :: [(T.Text, UID)]
    }

envParent :: Lens' Env (Maybe UID)
envParent f env = (\x -> env {_envParent = x}) <$> f (_envParent env)

envNext :: Lens' Env (Maybe UID)
envNext f env = (\x -> env {_envNext = x}) <$> f (_envNext env)

withParent :: Maybe UID -> Blocky a -> Blocky a
withParent = local . set envParent

withNext :: Maybe UID -> Blocky a -> Blocky a
withNext = local . set envNext

type Blocky = RWST Env [(UID, JValue)] UIDState (Except BlockError)

procToBlocks :: Env -> Procedure -> Except BlockError [(T.Text, JValue)]
procToBlocks env proc = do
  let name = proc ^. procedureName
  params <-
    for (proc ^. procedureParams) $ \case
      Sym sym -> return sym
      _ -> throwError $ NonSymbolInProcDef name
  let env' = env {_envProcArgs = params}
  snd <$> evalRWST (bProc proc) env' ([], 0)

bProc :: Procedure -> Blocky ()
bProc (Procedure "when-flag-clicked" params body) = do
  unless (null params) $
    throwError $ InvalidParamsForSpecialProcDef "when-flag-clicked"
  this <- newID
  withParent (Just this) $ do
    (bodyID, _) <- bStmts body
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "event_whenflagclicked")
            , ("next", idJSON bodyID)
            , ("parent", JNull)
            , ("topLevel", JBool True)
            , ("x", JNum 0)
            , ("y", JNum 0)
            ])
      ]
bProc (Procedure name params body) = do
  this <- newID
  (bodyID, _) <- withParent (Just this) $ bStmts body
  protoypeID <- newID
  tell
    [ ( this
      , JObj
          [ ("opcode", JStr "procedures_definition")
          , ("next", idJSON bodyID)
          , ("parent", JNull)
          , ( "inputs"
            , JObj [("custom_block", JArr [JNum 1, idJSON $ Just protoypeID])])
          ])
    , ( protoypeID
      , JObj
          [ ("opcode", JStr "procedures_prototype")
          , ("next", JNull)
          , ("parent", idJSON $ Just this)
          , ("input", JObj [])
          , ("fields", JObj [])
          , ("shadow", JBool True)
          , ( "mutation"
            , JObj
                [ ("tagName", JStr "mutation")
                , ("children", JArr [])
                , ("proccode", JStr name)
                , ("argumentids", JStr "[]")
                , ("argumentnames", JStr "[]")
                , ("argumentdefaults", JStr "[]")
                , ("warp", JBool True)
                ])
          ])
    ]

bStmt :: Statement -> Blocky (Maybe UID, Maybe UID)
bStmt (ProcCall procName args) =
  case lookup procName builtinProcs of
    Just fn -> fn args
    Nothing -> do
      exisitingProcs <- asks _envProcs
      unless (procName `elem` exisitingProcs) $
        throwError $ UnknownProc procName
      this <- newID
      next <- asks _envNext
      parent <- asks _envParent
      tell
        [ ( this
          , JObj
              [ ("opcode", JStr "procedures_call")
              , ("next", idJSON next)
              , ("parent", idJSON parent)
              , ("inputs", JObj [])
              , ("fields", JObj [])
              , ( "mutation"
                , JObj
                    [ ("tagName", JStr "mutation")
                    , ("children", JArr [])
                    , ("proccode", JStr procName)
                    , ("argumentids", JStr "[]")
                    , ("warp", JBool True)
                    ])
              ])
        ]
      return (Just this, Just this)
bStmt (Do xs) = bStmts xs
bStmt (IfElse cond true false) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    condition <- bExpr cond
    (trueID, _) <- bStmt true
    (falseID, _) <- bStmt false
    next <- asks _envNext
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_ifthenelse")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ( "inputs"
              , JObj
                  [ ("CONDITION", condition)
                  , ("SUBSTACK", idJSON trueID)
                  , ("SUBSTACK2", idJSON falseID)
                  ])
            ])
      ]
    return (Just this, Just this)
bStmt (Repeat times body) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    times' <- bExpr times
    (bodyID, _) <- bStmts body
    next <- asks _envNext
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_repeat")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ("inputs", JObj [("TIMES", times'), ("SUBSTACK", idJSON bodyID)])
            ])
      ]
    return (Just this, Just this)
bStmt (Forever body) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    (bodyID, _) <- bStmts body
    next <- asks _envNext
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_forever")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ("inputs", JObj [("SUBSTACK", idJSON bodyID)])
            ])
      ]
    return (Just this, Just this)
bStmt (Until cond body) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    condition <- bExpr cond
    (bodyID, _) <- bStmts body
    next <- asks _envNext
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_repeat_until")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ( "inputs"
              , JObj [("CODITION", condition), ("SUBSTACK", idJSON bodyID)])
            ])
      ]
    return (Just this, Just this)
bStmt (While cond body) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    condition <- bExpr cond
    (bodyID, _) <- bStmts body
    next <- asks _envNext
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_while")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ( "inputs"
              , JObj [("CODITION", condition), ("SUBSTACK", idJSON bodyID)])
            ])
      ]
    return (Just this, Just this)
bStmt (For var times body) = do
  this <- newID
  parent <- asks _envParent
  next <- asks _envNext
  localVars <- asks _envLocalVars
  globalVars <- asks _envGlobalVars
  withParent (Just this) $ do
    sym <-
      case var of
        Sym sym -> return sym
        _ -> throwError $ InvalidArgsForBuiltinProc "for"
    var' <-
      case lookup sym (localVars ++ globalVars) of
        Just i -> return i
        Nothing -> throwError $ InvalidArgsForBuiltinProc "for"
    times' <- bExpr times
    (body', _) <- bStmts body
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_for_each")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ("inputs", JObj [("VALUE", times'), ("SUBSTACK", idJSON body')])
            , ("fields", JObj [("VARIABLE", JArr [JStr sym, JStr var'])])
            ])
      ]
    return (Just this, Just this)

builtinProcs :: [(T.Text, [Expr] -> Blocky (Maybe UID, Maybe UID))]
builtinProcs =
  (\(name, opcode, fields) -> (name, stackBlock opcode fields name)) <$>
  [ ("erase-all", "pen_clear", [])
  , ("stamp", "pen_stamp", [])
  , ("pen-down", "pen_penDown", [])
  , ("pen-up", "pen_penUp", [])
  , ("set-xy", "motion_gotoxy", [val "X", val "Y"])
  , ("set-size", "looks_setsizeto", [val "SIZE"])
  , ("set-costume", "looks_switchcostumeto", [val "COSTUME"])
  , ("show", "looks_show", [])
  , ("hide", "looks_hide", [])
  , ("change-x", "motion_changexby", [val "X"])
  , ("change-y", "motion_changexby", [val "Y"])
  , ("set-x", "motion_setx", [val "X"])
  , ("set-y", "motion_sety", [val "Y"])
  ]
  where
    val = (, bExpr)

stackBlock ::
     T.Text
  -> [(T.Text, Expr -> Blocky JValue)]
  -> T.Text
  -> [Expr]
  -> Blocky (Maybe UID, Maybe UID)
stackBlock opcode fieldFns procName args
  | length args /= length fieldFns =
    throwError $ InvalidArgsForBuiltinProc procName
  | otherwise = do
    let (fieldNames, fns) = unzip fieldFns
    this <- newID
    next <- asks _envNext
    parent <- asks _envParent
    fieldVals <- withParent (Just this) $ zipWithM ($) fns args
    let fields = zip fieldNames fieldVals
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr opcode)
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ("inputs", JObj fields)
            ])
      ]
    return (Just this, Just this)

bStmts :: [Statement] -> Blocky (Maybe UID, Maybe UID)
bStmts [] = asks $ (Nothing, ) <$> _envParent
bStmts (x:xs) = do
  next <- newID
  (firstStart, firstEnd) <- withNext (Just next) $ bStmt x
  prependID next
  (_, restEnd) <- withParent firstEnd $ bStmts xs
  return (firstStart, restEnd)

bExpr :: Expr -> Blocky JValue
bExpr (Lit (VNum num)) = return $ JArr [JNum 1, JArr [JNum 4, JNum num]]
bExpr (Lit (VStr str)) = return $ JArr [JNum 1, JArr [JNum 10, JStr str]]
bExpr (Lit (VBool True)) = return $ JArr [JNum 1, JArr [JNum 10, JStr "true"]]
bExpr (Lit (VBool False)) = return $ JArr [JNum 1, JArr [JNum 10, JStr "false"]]
bExpr (Sym sym) = do
  env <- ask
  let procArgs = _envProcArgs env
  let vars = _envLocalVars env ++ _envGlobalVars env
  let lists = _envLocalLists env ++ _envGlobalLists env
  if sym `elem` procArgs
    then do
      this <- newID
      parent <- asks _envParent
      tell
        [ ( this
          , JObj
              [ ("opcode", JStr "argument_reporter_string_number")
              , ("parent", idJSON parent)
              , ("fields", JObj [("VALUE", JArr [JStr sym, JNull])])
              ])
        ]
      return $ JArr [JNum 1, JStr this]
    else case lookup sym vars of
           Just i -> return $ JArr [JNum 1, JArr [JNum 12, JStr sym, JStr i]]
           Nothing ->
             case lookup sym lists of
               Just i ->
                 return $ JArr [JNum 1, JArr [JNum 13, JStr sym, JStr i]]
               Nothing ->
                 case lookup sym builtinSymbols of
                   Just i -> i
                   Nothing -> throwError $ UnknownSymbolInExpr sym
bExpr (FuncCall "+" args) = go args
  where
    go [] = bExpr $ Lit $ VNum 0
    go [x] = bExpr x
    go (lhs:rhs) = do
      this <- newID
      parent <- asks _envParent
      withParent (Just this) $ do
        lhs' <- bExpr lhs
        rhs' <- go rhs
        tell
          [ ( this
            , JObj
                [ ("opcode", JStr "operator_add")
                , ("parent", idJSON parent)
                , ("inputs", JObj [("NUM1", lhs'), ("NUM2", rhs')])
                ])
          ]
        return $ JStr this
bExpr (FuncCall "-" []) = throwError $ FuncWrongArgCount "-" (AtLeast 1) 0
bExpr (FuncCall "-" [x]) = bExpr (FuncCall "-" [Lit (VNum 0), x])
bExpr (FuncCall "-" [lhs, rhs]) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    lhs' <- bExpr lhs
    rhs' <- bExpr rhs
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "operator_subtract")
            , ("parent", idJSON parent)
            , ("inputs", JObj [("NUM1", lhs'), ("NUM2", rhs')])
            ])
      ]
    return $ JStr this
bExpr (FuncCall "-" (x:xs)) = bExpr (FuncCall "-" [x, FuncCall "+" xs])
bExpr (FuncCall "*" args) = go args
  where
    go [] = bExpr $ Lit $ VNum 0
    go [x] = bExpr x
    go (lhs:rhs) = do
      this <- newID
      parent <- asks _envParent
      withParent (Just this) $ do
        lhs' <- bExpr lhs
        rhs' <- go rhs
        tell
          [ ( this
            , JObj
                [ ("opcode", JStr "operator_multiply")
                , ("parent", idJSON parent)
                , ("inputs", JObj [("NUM1", lhs'), ("NUM2", rhs')])
                ])
          ]
        return $ JStr this
bExpr (FuncCall "=" [lhs, rhs]) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    lhs' <- bExpr lhs
    rhs' <- bExpr rhs
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "operator_equal")
            , ("parent", idJSON parent)
            , ("inputs", JObj [("OPERAND1", lhs'), ("OPERAND2", rhs')])
            ])
      ]
    return $ JStr this
bExpr (FuncCall "=" args) =
  throwError $ FuncWrongArgCount "=" (Exactly 2) $ length args
bExpr (FuncCall "<" [lhs, rhs]) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    lhs' <- bExpr lhs
    rhs' <- bExpr rhs
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "operator_lt")
            , ("parent", idJSON parent)
            , ("inputs", JObj [("OPERAND1", lhs'), ("OPERAND2", rhs')])
            ])
      ]
    return $ JStr this
bExpr (FuncCall "<" args) =
  throwError $ FuncWrongArgCount "<" (Exactly 2) $ length args
bExpr (FuncCall "length" [str]) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    str' <- bExpr str
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "operator_lt")
            , ("parent", idJSON parent)
            , ("inputs", JObj [("STRING", str')])
            ])
      ]
    return $ JStr this
bExpr (FuncCall "length" args) =
  throwError $ FuncWrongArgCount "length" (Exactly 1) $ length args
bExpr (FuncCall name args) = error $ T.unpack name

builtinSymbols :: [(T.Text, Blocky JValue)]
builtinSymbols =
  [ ( "x-pos"
    , do this <- newID
         parent <- asks _envParent
         tell
           [ ( this
             , JObj
                 [ ("opcode", JStr "motion_xposition")
                 , ("parent", idJSON parent)
                 , ("inputs", JObj [])
                 ])
           ]
         return $ JStr this)
  ]
