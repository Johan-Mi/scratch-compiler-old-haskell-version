{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Block
  ( Env(..)
  , BlockError
  , procToBlocks
  ) where

import Block.Error (ArgCount(..), BlockError(..))
import Control.Monad (guard, unless, zipWithM)
import Control.Monad.Except (Except, throwError)
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Reader (ReaderT, ask, asks, local)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (tell)
import Data.Functor (($>), (<&>))
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..))
import qualified Data.Text as T
import Data.Traversable (for)
import JSON (JValue(..))
import Lens.Micro (Lens', set)
import Mid.Expr (Expr(..), Value(..))
import Mid.Proc (Procedure(..), Statement(..))
import UID (UID, UIDState, idJSON, newID, prependID)

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

envProcArgs :: Lens' Env [T.Text]
envProcArgs f env = (\x -> env {_envProcArgs = x}) <$> f (_envProcArgs env)

withParent :: Maybe UID -> Blocky a -> Blocky a
withParent = local . set envParent

withNext :: Maybe UID -> Blocky a -> Blocky a
withNext = local . set envNext

withProcArgs :: [T.Text] -> Blocky a -> Blocky a
withProcArgs = local . set envProcArgs

type Blocky = RWST Env [(UID, JValue)] UIDState (Except BlockError)

procToBlocks ::
     Procedure
  -> ReaderT Env (StateT UIDState (Except BlockError)) [(T.Text, JValue)]
procToBlocks proc = do
  env <- ask
  st <- get
  (_, s, w) <- lift $ lift $ runRWST (bProc proc) env st
  put s
  return w

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
  params' <-
    for params $ \case
      Sym sym -> return sym
      _ -> throwError $ NonSymbolInProcDef name
  this <- newID
  (bodyID, _) <- withParent (Just this) $ withProcArgs params' $ bStmts body
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
    (trueID, _) <- withNext Nothing $ bStmt true
    (falseID, _) <- withNext Nothing $ bStmt false
    next <- asks _envNext
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_if_else")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ( "inputs"
              , JObj
                  [ ("CONDITION", condition)
                  , ("SUBSTACK", JArr [JNum 2, idJSON trueID])
                  , ("SUBSTACK2", JArr [JNum 2, idJSON falseID])
                  ])
            ])
      ]
    return (Just this, Just this)
bStmt (Repeat times body) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    times' <- bExpr times
    (bodyID, _) <- withNext Nothing $ bStmts body
    next <- asks _envNext
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_repeat")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ( "inputs"
              , JObj
                  [ ("TIMES", times')
                  , ("SUBSTACK", JArr [JNum 2, idJSON bodyID])
                  ])
            ])
      ]
    return (Just this, Just this)
bStmt (Forever body) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    (bodyID, _) <- withNext Nothing $ bStmts body
    next <- asks _envNext
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_forever")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ("inputs", JObj [("SUBSTACK", JArr [JNum 2, idJSON bodyID])])
            ])
      ]
    return (Just this, Just this)
bStmt (Until cond body) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    condition <- bExpr cond
    (bodyID, _) <- withNext Nothing $ bStmts body
    next <- asks _envNext
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_repeat_until")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ( "inputs"
              , JObj
                  [ ("CODITION", condition)
                  , ("SUBSTACK", JArr [JNum 2, idJSON bodyID])
                  ])
            ])
      ]
    return (Just this, Just this)
bStmt (While cond body) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    condition <- bExpr cond
    (bodyID, _) <- withNext Nothing $ bStmts body
    next <- asks _envNext
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_while")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ( "inputs"
              , JObj
                  [ ("CODITION", condition)
                  , ("SUBSTACK", JArr [JNum 2, idJSON bodyID])
                  ])
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
    (body', _) <- withNext Nothing $ bStmts body
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_for_each")
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ( "inputs"
              , JObj
                  [("VALUE", times'), ("SUBSTACK", JArr [JNum 2, idJSON body'])])
            , ("fields", JObj [("VARIABLE", JArr [JStr sym, JStr var'])])
            ])
      ]
    return (Just this, Just this)

builtinProcs :: [(T.Text, [Expr] -> Blocky (Maybe UID, Maybe UID))]
builtinProcs =
  ((\(name, opcode, fields) -> (name, stackBlock opcode fields name)) <$>
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
   , ("send-broadcast-sync", "event_broadcastandwait", [val "BROADCAST_INPUT"])
   ]) ++
  [ ( ":="
    , \case
        [Sym varName, value] -> do
          this <- newID
          next <- asks _envNext
          parent <- asks _envParent
          variable' <- varField varName
          value' <- withParent (Just this) $ bExpr value
          tell
            [ ( this
              , JObj
                  [ ("opcode", JStr "data_setvariableto")
                  , ("next", idJSON next)
                  , ("parent", idJSON parent)
                  , ("inputs", JObj [("VALUE", value')])
                  , ("fields", JObj [("VARIABLE", variable')])
                  ])
            ]
          return (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc ":=")
  , ( "+="
    , \case
        [Sym varName, value] -> do
          this <- newID
          next <- asks _envNext
          parent <- asks _envParent
          variable' <- varField varName
          value' <- withParent (Just this) $ bExpr value
          tell
            [ ( this
              , JObj
                  [ ("opcode", JStr "data_changevariableby")
                  , ("next", idJSON next)
                  , ("parent", idJSON parent)
                  , ("inputs", JObj [("VALUE", value')])
                  , ("fields", JObj [("VARIABLE", variable')])
                  ])
            ]
          return (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "+=")
  , ( "replace"
    , \case
        [Sym listName, index, value] -> do
          this <- newID
          next <- asks _envNext
          parent <- asks _envParent
          list' <- listField listName
          withParent (Just this) $ do
            index' <- bExpr index
            value' <- bExpr value
            tell
              [ ( this
                , JObj
                    [ ("opcode", JStr "data_replaceitemoflist")
                    , ("next", idJSON next)
                    , ("parent", idJSON parent)
                    , ("inputs", JObj [("INDEX", index'), ("VALUE", value')])
                    , ("fields", JObj [("LIST", list')])
                    ])
              ]
            return (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "replace")
  , ( "append"
    , \case
        [Sym listName, value] -> do
          this <- newID
          next <- asks _envNext
          parent <- asks _envParent
          list' <- listField listName
          value' <- withParent (Just this) $ bExpr value
          tell
            [ ( this
              , JObj
                  [ ("opcode", JStr "data_addtolist")
                  , ("next", idJSON next)
                  , ("parent", idJSON parent)
                  , ("inputs", JObj [("VALUE", value')])
                  , ("fields", JObj [("LIST", list')])
                  ])
            ]
          return (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "append")
  , ( "delete"
    , \case
        [Sym listName, index] -> do
          this <- newID
          next <- asks _envNext
          parent <- asks _envParent
          list' <- listField listName
          index' <- withParent (Just this) $ bExpr index
          tell
            [ ( this
              , JObj
                  [ ("opcode", JStr "data_deleteoflist")
                  , ("next", idJSON next)
                  , ("parent", idJSON parent)
                  , ("inputs", JObj [("INDEX", index')])
                  , ("fields", JObj [("LIST", list')])
                  ])
            ]
          return (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "delete")
  , ( "delete-all"
    , \case
        [Sym listName] -> do
          this <- newID
          next <- asks _envNext
          parent <- asks _envParent
          list' <- listField listName
          tell
            [ ( this
              , JObj
                  [ ("opcode", JStr "data_deletealloflist")
                  , ("next", idJSON next)
                  , ("parent", idJSON parent)
                  , ("fields", JObj [("LIST", list')])
                  ])
            ]
          return (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "delete-all")
  , ( "stop-all"
    , \case
        [] -> do
          this <- newID
          next <- asks _envNext
          parent <- asks _envParent
          tell
            [ ( this
              , JObj
                  [ ("opcode", JStr "control_stop")
                  , ("next", idJSON next)
                  , ("parent", idJSON parent)
                  , ( "inputs"
                    , JObj
                        [ ( "STOP_OPTION"
                          , JArr [JNum 1, JArr [JNum 10, JStr "all"]])
                        ])
                  ])
            ]
          return (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "stop-all")
  , ( "stop-this-script"
    , \case
        [] -> do
          this <- newID
          next <- asks _envNext
          parent <- asks _envParent
          tell
            [ ( this
              , JObj
                  [ ("opcode", JStr "control_stop")
                  , ("next", idJSON next)
                  , ("parent", idJSON parent)
                  , ( "inputs"
                    , JObj
                        [ ( "STOP_OPTION"
                          , JArr [JNum 1, JArr [JNum 10, JStr "this script"]])
                        ])
                  ])
            ]
          return (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "stop-this-script")
  ]
  where
    val = (, bExpr)

varField :: T.Text -> Blocky JValue
varField name = do
  localVars <- asks _envLocalVars
  globalVars <- asks _envGlobalVars
  let vars = localVars ++ globalVars
  case lookup name vars of
    Just varID -> return $ JArr [JStr name, JStr varID]
    Nothing -> throwError $ VarDoesntExist name

listField :: T.Text -> Blocky JValue
listField name = do
  localLists <- asks _envLocalLists
  globalLists <- asks _envGlobalLists
  let lists = localLists ++ globalLists
  case lookup name lists of
    Just listID -> return $ JArr [JStr name, JStr listID]
    Nothing -> throwError $ ListDoesntExist name

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
bStmts [] = asks $ (Nothing, ) . _envParent
bStmts [x] = bStmt x
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
      vars = _envLocalVars env ++ _envGlobalVars env
      lists = _envLocalLists env ++ _envGlobalLists env
      theProcArg =
        guard (sym `elem` procArgs) $> do
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
      theVar =
        lookup sym vars <&> \i ->
          return $ JArr [JNum 1, JArr [JNum 12, JStr sym, JStr i]]
      theList =
        lookup sym lists <&> \i ->
          return $ JArr [JNum 1, JArr [JNum 13, JStr sym, JStr i]]
      theBuiltin = lookup sym builtinSymbols
  fromMaybe err $
    getFirst $ foldMap First [theProcArg, theVar, theList, theBuiltin]
  where
    err = throwError $ UnknownSymbolInExpr sym
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
        return $ JArr [JNum 1, JStr this]
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
    return $ JArr [JNum 1, JStr this]
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
        return $ JArr [JNum 1, JStr this]
bExpr (FuncCall "=" [lhs, rhs]) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    lhs' <- bExpr lhs
    rhs' <- bExpr rhs
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "operator_equals")
            , ("parent", idJSON parent)
            , ("inputs", JObj [("OPERAND1", lhs'), ("OPERAND2", rhs')])
            ])
      ]
    return $ JArr [JNum 1, JStr this]
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
    return $ JArr [JNum 1, JStr this]
bExpr (FuncCall "<" args) =
  throwError $ FuncWrongArgCount "<" (Exactly 2) $ length args
bExpr (FuncCall "str-length" [str]) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    str' <- bExpr str
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "operator_length")
            , ("parent", idJSON parent)
            , ("inputs", JObj [("STRING", str')])
            ])
      ]
    return $ JArr [JNum 1, JStr this]
bExpr (FuncCall "str-length" args) =
  throwError $ FuncWrongArgCount "str-length" (Exactly 1) $ length args
bExpr (FuncCall "length" [listName]) = do
  this <- newID
  parent <- asks _envParent
  list' <-
    case listName of
      Sym sym -> listField sym
      _ -> throwError $ InvalidArgsForBuiltinFunc "length"
  tell
    [ ( this
      , JObj
          [ ("opcode", JStr "data_lengthoflist")
          , ("parent", idJSON parent)
          , ("fields", JObj [("LIST", list')])
          ])
    ]
  return $ JArr [JNum 1, JStr this]
bExpr (FuncCall "length" args) =
  throwError $ FuncWrongArgCount "length" (Exactly 1) $ length args
bExpr (FuncCall "++" args) = go args
  where
    go [] = bExpr $ Lit $ VStr ""
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
                [ ("opcode", JStr "operator_join")
                , ("parent", idJSON parent)
                , ("inputs", JObj [("STRING1", lhs'), ("STRING2", rhs')])
                ])
          ]
        return $ JArr [JNum 1, JStr this]
bExpr (FuncCall "not" [arg]) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    arg' <- bExpr arg
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "operator_not")
            , ("parent", idJSON parent)
            , ("inputs", JObj [("OPERAND", arg')])
            ])
      ]
    return $ JArr [JNum 1, JStr this]
bExpr (FuncCall "not" args) =
  throwError $ FuncWrongArgCount "not" (Exactly 1) $ length args
bExpr (FuncCall "char-at" [str, index]) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    str' <- bExpr str
    index' <- bExpr index
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "operator_letter_of")
            , ("parent", idJSON parent)
            , ("inputs", JObj [("STRING", str'), ("INDEX", index')])
            ])
      ]
    return $ JArr [JNum 1, JStr this]
bExpr (FuncCall "char-at" args) =
  throwError $ FuncWrongArgCount "char-at" (Exactly 2) $ length args
bExpr (FuncCall "mod" [lhs, rhs]) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    lhs' <- bExpr lhs
    rhs' <- bExpr rhs
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "operator_mod")
            , ("parent", idJSON parent)
            , ("inputs", JObj [("NUM1", lhs'), ("NUM2", rhs')])
            ])
      ]
    return $ JArr [JNum 1, JStr this]
bExpr (FuncCall "mod" args) =
  throwError $ FuncWrongArgCount "mod" (Exactly 2) $ length args
bExpr (FuncCall "!!" [list, index]) = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ do
    list' <-
      case list of
        Sym sym -> listField sym
        _ -> throwError $ InvalidArgsForBuiltinFunc "!!"
    index' <- bExpr index
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "data_itemoflist")
            , ("parent", idJSON parent)
            , ("inputs", JObj [("INDEX", index')])
            , ("fields", JObj [("LIST", list')])
            ])
      ]
    return $ JArr [JNum 1, JStr this]
bExpr (FuncCall "!!" args) =
  throwError $ FuncWrongArgCount "!!" (Exactly 2) $ length args
bExpr (FuncCall name _) = throwError $ UnknownFunc name

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
         return $ JArr [JNum 1, JStr this])
  ]
