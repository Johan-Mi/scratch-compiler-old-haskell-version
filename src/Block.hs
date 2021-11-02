{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Block
  ( Env(..)
  , BlockError
  , procToBlocks
  ) where

import Block.Env (Env(..), withNext, withParent, withProcArgs)
import Block.Error (ArgCount(..), BlockError(..))
import Control.Monad (guard, unless, zipWithM)
import Control.Monad.Except (Except, throwError)
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Reader (ReaderT, ask, asks, local)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (tell)
import Data.Foldable (fold)
import Data.Functor (($>), (<&>))
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Monoid (First(..))
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Traversable (for)
import JSON (JValue(..), showJSON)
import Mid.Expr (Expr(..), Value(..), toString)
import Mid.Proc (Procedure(..), Statement(..))
import UID (UID, UIDState, idJSON, newID, prependID)

type Blocky = RWST Env [(UID, JValue)] UIDState (Except BlockError)

data Reporter
  = Shadow JValue
  | NonShadow JValue

noShadow :: Reporter -> JValue
noShadow (Shadow jv) = JArr [JNum 1, jv]
noShadow (NonShadow jv) = JArr [JNum 2, jv]

withShadow :: JValue -> Reporter -> JValue
withShadow _ (Shadow jv) = JArr [JNum 1, jv]
withShadow obscured (NonShadow jv) = JArr [JNum 3, jv, obscured]

emptyShadow :: Reporter -> JValue
emptyShadow = withShadow $ JArr [JNum 10, JStr ""]

procToBlocks ::
     Procedure
  -> ReaderT Env (StateT UIDState (Except BlockError)) [(T.Text, JValue)]
procToBlocks proc = do
  env <- ask
  st <- get
  (_, s, w) <- lift $ lift $ runRWST (bProc proc) env st
  put s
  pure w

bProc :: Procedure -> Blocky ()
bProc (Procedure "when-flag-clicked" params body vars lists) = do
  unless (null params) $
    throwError $ InvalidParamsForSpecialProcDef "when-flag-clicked"
  this <- newID
  withParent (Just this) $ do
    (bodyID, _) <- withLocals vars lists $ bStmts body
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
bProc (Procedure "when-cloned" params body vars lists) = do
  unless (null params) $
    throwError $ InvalidParamsForSpecialProcDef "when-cloned"
  this <- newID
  withParent (Just this) $ do
    (bodyID, _) <- withLocals vars lists $ bStmts body
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "control_start_as_clone")
            , ("next", idJSON bodyID)
            , ("parent", JNull)
            , ("topLevel", JBool True)
            , ("x", JNum 0)
            , ("y", JNum 0)
            ])
      ]
bProc (Procedure "when-received" params body vars lists) = do
  name <-
    case params of
      [Lit (VStr name')] -> pure name'
      _ -> throwError $ InvalidParamsForSpecialProcDef "when-received"
  this <- newID
  withParent (Just this) $ do
    (bodyID, _) <- withLocals vars lists $ bStmts body
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr "event_whenbroadcastreceived")
            , ("next", idJSON bodyID)
            , ("parent", JNull)
            , ("fields", JObj [("BROADCAST_OPTION", JArr [JStr name, JNull])])
            , ("topLevel", JBool True)
            , ("x", JNum 0)
            , ("y", JNum 0)
            ])
      ]
bProc (Procedure name params body vars lists) = do
  params' <-
    for params $ \case
      Sym sym -> pure sym
      _ -> throwError $ NonSymbolInProcDef name
  this <- newID
  withParent (Just this) $
    withProcArgs params' $
    withLocals vars lists $ do
      (bodyID, _) <- bStmts body
      exisitingProcs <- asks _envProcs
      protoypeID <- newID
      let paramIDs = snd <$> fromJust (lookup name exisitingProcs)
      reporters <-
        fmap noShadow <$> withParent (Just protoypeID) (traverse bExpr params)
      let argumentids =
            toStrict $ decodeUtf8 $ showJSON $ JArr $ JStr <$> paramIDs
      let argumentnames =
            toStrict $ decodeUtf8 $ showJSON $ JArr $ JStr <$> params'
      let argumentdefaults =
            toStrict $ decodeUtf8 $ showJSON $ JArr $ params' $> JStr ""
      let proccode = T.append name $ T.replicate (length params) " %s"
      tell
        [ ( this
          , JObj
              [ ("opcode", JStr "procedures_definition")
              , ("next", idJSON bodyID)
              , ("parent", JNull)
              , ( "inputs"
                , JObj
                    [("custom_block", JArr [JNum 1, idJSON $ Just protoypeID])])
              , ("topLevel", JBool True)
              , ("x", JNum 0)
              , ("y", JNum 0)
              ])
        , ( protoypeID
          , JObj
              [ ("opcode", JStr "procedures_prototype")
              , ("next", JNull)
              , ("parent", idJSON $ Just this)
              , ("inputs", JObj $ zip paramIDs reporters)
              , ("fields", JObj [])
              , ("shadow", JBool True)
              , ( "mutation"
                , JObj
                    [ ("tagName", JStr "mutation")
                    , ("children", JArr [])
                    , ("proccode", JStr proccode)
                    , ("argumentids", JStr argumentids)
                    , ("argumentnames", JStr argumentnames)
                    , ("argumentdefaults", JStr argumentdefaults)
                    , ("warp", JStr "true")
                    ])
              ])
        ]

bStmt :: Statement -> Blocky (Maybe UID, Maybe UID)
bStmt (ProcCall procName args) =
  case lookup procName builtinProcs of
    Just fn -> fn args
    Nothing ->
      boil $ \this parent -> do
        exisitingProcs <- asks _envProcs
        paramIDs <-
          maybe (throwError $ UnknownProc procName) (pure . fmap snd) $
          lookup procName exisitingProcs
        next <- asks _envNext
        args' <- fmap emptyShadow <$> traverse bExpr args
        let inputs = zip paramIDs args'
        let proccode = T.append procName $ T.replicate (length args) " %s"
        let argumentids =
              toStrict $ decodeUtf8 $ showJSON $ JArr $ JStr <$> paramIDs
        tell
          [ ( this
            , JObj
                [ ("opcode", JStr "procedures_call")
                , ("next", idJSON next)
                , ("parent", idJSON parent)
                , ("inputs", JObj inputs)
                , ("fields", JObj [])
                , ( "mutation"
                  , JObj
                      [ ("tagName", JStr "mutation")
                      , ("children", JArr [])
                      , ("proccode", JStr proccode)
                      , ("argumentids", JStr argumentids)
                      , ("warp", JStr "true")
                      ])
                ])
          ]
        pure (Just this, Just this)
bStmt (Do xs) = bStmts xs
bStmt (IfElse cond true false) =
  boil $ \this parent -> do
    condition <- noShadow <$> bExpr cond
    (trueID, _) <- withNext Nothing $ bStmt true
    (falseID, _) <- withNext Nothing $ bStmt false
    next <- asks _envNext
    if isNothing falseID
      then tell
             [ ( this
               , JObj
                   [ ("opcode", JStr "control_if")
                   , ("next", idJSON next)
                   , ("parent", idJSON parent)
                   , ( "inputs"
                     , JObj
                         [ ("CONDITION", condition)
                         , ("SUBSTACK", JArr [JNum 2, idJSON trueID])
                         ])
                   ])
             ]
      else tell
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
    pure (Just this, Just this)
bStmt (Repeat times body) =
  boil $ \this parent -> do
    times' <- emptyShadow <$> bExpr times
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
    pure (Just this, Just this)
bStmt (Forever body) =
  boil $ \this parent -> do
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
    pure (Just this, Just this)
bStmt (Until cond body) =
  boil $ \this parent -> do
    condition <- noShadow <$> bExpr cond
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
    pure (Just this, Just this)
bStmt (While cond body) =
  boil $ \this parent -> do
    condition <- noShadow <$> bExpr cond
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
    pure (Just this, Just this)
bStmt (For var times body) =
  boil $ \this parent -> do
    next <- asks _envNext
    var' <-
      case var of
        Sym sym -> varField sym
        _ -> throwError $ InvalidArgsForBuiltinProc "for"
    times' <- emptyShadow <$> bExpr times
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
            , ("fields", JObj [("VARIABLE", var')])
            ])
      ]
    pure (Just this, Just this)

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
   , ("change-x", "motion_changexby", [val "DX"])
   , ("change-y", "motion_changeyby", [val "DY"])
   , ("set-x", "motion_setx", [val "X"])
   , ("set-y", "motion_sety", [val "Y"])
   , ("wait", "control_wait", [val "DURATION"])
   ]) ++
  [ ( "send-broadcast-sync"
    , \case
        [name] ->
          boil $ \this parent -> do
            next <- asks _envNext
            name' <-
              case name of
                Lit lit ->
                  pure $
                  JArr [JNum 1, JArr [JNum 11, JStr (toString lit), JStr ""]]
                n -> noShadow <$> bExpr n
            tell
              [ ( this
                , JObj
                    [ ("opcode", JStr "event_broadcastandwait")
                    , ("next", idJSON next)
                    , ("parent", idJSON parent)
                    , ("inputs", JObj [("BROADCAST_INPUT", name')])
                    ])
              ]
            pure (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "send-broadcast-sync")
  , ( ":="
    , \case
        [Sym varName, value] ->
          boil $ \this parent -> do
            next <- asks _envNext
            variable' <- varField varName
            value' <- emptyShadow <$> bExpr value
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
            pure (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc ":=")
  , ( "+="
    , \case
        [Sym varName, value] ->
          boil $ \this parent -> do
            next <- asks _envNext
            variable' <- varField varName
            value' <- emptyShadow <$> bExpr value
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
            pure (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "+=")
  , ( "replace"
    , \case
        [Sym listName, index, item] ->
          boil $ \this parent -> do
            next <- asks _envNext
            list' <- listField listName
            index' <- emptyShadow <$> withParent (Just this) (bExpr index)
            item' <- emptyShadow <$> bExpr item
            tell
              [ ( this
                , JObj
                    [ ("opcode", JStr "data_replaceitemoflist")
                    , ("next", idJSON next)
                    , ("parent", idJSON parent)
                    , ("inputs", JObj [("INDEX", index'), ("ITEM", item')])
                    , ("fields", JObj [("LIST", list')])
                    ])
              ]
            pure (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "replace")
  , ( "append"
    , \case
        [Sym listName, value] ->
          boil $ \this parent -> do
            next <- asks _envNext
            list' <- listField listName
            value' <- emptyShadow <$> bExpr value
            tell
              [ ( this
                , JObj
                    [ ("opcode", JStr "data_addtolist")
                    , ("next", idJSON next)
                    , ("parent", idJSON parent)
                    , ("inputs", JObj [("ITEM", value')])
                    , ("fields", JObj [("LIST", list')])
                    ])
              ]
            pure (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "append")
  , ( "delete"
    , \case
        [Sym listName, index] ->
          boil $ \this parent -> do
            next <- asks _envNext
            list' <- listField listName
            index' <- emptyShadow <$> bExpr index
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
            pure (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "delete")
  , ( "delete-all"
    , \case
        [Sym listName] ->
          boil $ \this parent -> do
            next <- asks _envNext
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
            pure (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "delete-all")
  , ( "stop-all"
    , \case
        [] ->
          boil $ \this parent -> do
            next <- asks _envNext
            tell
              [ ( this
                , JObj
                    [ ("opcode", JStr "control_stop")
                    , ("next", idJSON next)
                    , ("parent", idJSON parent)
                    , ( "fields"
                      , JObj [("STOP_OPTION", JArr [JStr "all", JNull])])
                    ])
              ]
            pure (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "stop-all")
  , ( "stop-this-script"
    , \case
        [] ->
          boil $ \this parent -> do
            next <- asks _envNext
            tell
              [ ( this
                , JObj
                    [ ("opcode", JStr "control_stop")
                    , ("next", idJSON next)
                    , ("parent", idJSON parent)
                    , ( "fields"
                      , JObj [("STOP_OPTION", JArr [JStr "this script", JNull])])
                    ])
              ]
            pure (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "stop-this-script")
  , ( "clone-myself"
    , \case
        [] ->
          boil $ \this parent -> do
            next <- asks _envNext
            menu <- newID
            tell
              [ ( this
                , JObj
                    [ ("opcode", JStr "control_create_clone_of")
                    , ("next", idJSON next)
                    , ("parent", idJSON parent)
                    , ( "inputs"
                      , JObj [("CLONE_OPTION", JArr [JNum 1, JStr menu])])
                    ])
              , ( menu
                , JObj
                    [ ("opcode", JStr "control_create_clone_of_menu")
                    , ("parent", JStr this)
                    , ( "fields"
                      , JObj [("CLONE_OPTION", JArr [JStr "_myself_", JNull])])
                    , ("shadow", JBool True)
                    ])
              ]
            pure (Just this, Just this)
        _ -> throwError $ InvalidArgsForBuiltinProc "clone-myself")
  ]
  where
    val = (, fmap emptyShadow . bExpr)

varField :: T.Text -> Blocky JValue
varField name = do
  vars <- asks $ fold [_envLocalVars, _envSpriteVars, _envGlobalVars]
  case lookup name vars of
    Just (varID, name') -> pure $ JArr [JStr name', JStr varID]
    Nothing -> throwError $ VarDoesntExist name

listField :: T.Text -> Blocky JValue
listField name = do
  lists <- asks $ fold [_envLocalLists, _envSpriteLists, _envGlobalLists]
  case lookup name lists of
    Just (listID, name') -> pure $ JArr [JStr name', JStr listID]
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
  | otherwise =
    boil $ \this parent -> do
      let (fieldNames, fns) = unzip fieldFns
      next <- asks _envNext
      fieldVals <- zipWithM ($) fns args
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
      pure (Just this, Just this)

bStmts :: [Statement] -> Blocky (Maybe UID, Maybe UID)
bStmts [] = asks $ (Nothing, ) . _envParent
bStmts [x] = bStmt x
bStmts (x:xs) = do
  this <- newID
  next <- newID
  prependID this
  (firstStart, firstEnd) <- withNext (Just next) $ bStmt x
  prependID next
  (_, restEnd) <- withParent firstEnd $ bStmts xs
  pure (firstStart, restEnd)

bExpr :: Expr -> Blocky Reporter
bExpr (Lit lit) = pure $ Shadow $ JArr [JNum 10, JStr $ toString lit]
bExpr (Sym sym) = do
  env <- ask
  let procArgs = _envProcArgs env
      vars = fold [_envLocalVars, _envSpriteVars, _envGlobalVars] env
      lists = fold [_envLocalLists, _envSpriteLists, _envGlobalLists] env
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
          pure $ NonShadow $ JStr this
      theVar =
        lookup sym vars <&> \(i, name) ->
          pure $ NonShadow $ JArr [JNum 12, JStr name, JStr i]
      theList =
        lookup sym lists <&> \(i, name) ->
          pure $ NonShadow $ JArr [JNum 13, JStr name, JStr i]
      theBuiltin = lookup sym builtinSymbols
  fromMaybe err $
    getFirst $ foldMap First [theProcArg, theVar, theList, theBuiltin]
  where
    err = throwError $ UnknownSymbolInExpr sym
bExpr (FuncCall name args) =
  maybe (throwError $ UnknownFunc name) ($ args) $ lookup name builtinFuncs

builtinFuncs :: [(T.Text, [Expr] -> Blocky Reporter)]
builtinFuncs =
  [ ( "!!"
    , \case
        [list, index] ->
          boil $ \this parent -> do
            list' <-
              case list of
                Sym sym -> listField sym
                _ -> throwError $ InvalidArgsForBuiltinFunc "!!"
            index' <- emptyShadow <$> bExpr index
            tell
              [ ( this
                , JObj
                    [ ("opcode", JStr "data_itemoflist")
                    , ("parent", idJSON parent)
                    , ("inputs", JObj [("INDEX", index')])
                    , ("fields", JObj [("LIST", list')])
                    ])
              ]
            pure $ NonShadow $ JStr this
        args -> throwError $ FuncWrongArgCount "!!" (Exactly 2) $ length args)
  , ( "+"
    , let go [] = bExpr $ Lit $ VNum 0
          go [x] = bExpr x
          go (lhs:rhs) =
            boil $ \this parent -> do
              lhs' <- emptyShadow <$> bExpr lhs
              rhs' <- emptyShadow <$> go rhs
              tell
                [ ( this
                  , JObj
                      [ ("opcode", JStr "operator_add")
                      , ("parent", idJSON parent)
                      , ("inputs", JObj [("NUM1", lhs'), ("NUM2", rhs')])
                      ])
                ]
              pure $ NonShadow $ JStr this
       in go)
  , ( "-"
    , \case
        [] -> throwError $ FuncWrongArgCount "-" (AtLeast 1) 0
        [x] -> bExpr (FuncCall "-" [Lit (VNum 0), x])
        (lhs:rhs) ->
          boil $ \this parent -> do
            lhs' <- emptyShadow <$> bExpr lhs
            rhs' <- emptyShadow <$> bExpr (FuncCall "+" rhs)
            tell
              [ ( this
                , JObj
                    [ ("opcode", JStr "operator_subtract")
                    , ("parent", idJSON parent)
                    , ("inputs", JObj [("NUM1", lhs'), ("NUM2", rhs')])
                    ])
              ]
            pure $ NonShadow $ JStr this)
  , ( "*"
    , let go [] = bExpr $ Lit $ VNum 0
          go [x] = bExpr x
          go (lhs:rhs) =
            boil $ \this parent -> do
              lhs' <- emptyShadow <$> bExpr lhs
              rhs' <- emptyShadow <$> go rhs
              tell
                [ ( this
                  , JObj
                      [ ("opcode", JStr "operator_multiply")
                      , ("parent", idJSON parent)
                      , ("inputs", JObj [("NUM1", lhs'), ("NUM2", rhs')])
                      ])
                ]
              pure $ NonShadow $ JStr this
       in go)
  , ( "/"
    , \case
        (lhs:rhs@(_:_)) ->
          boil $ \this parent -> do
            lhs' <- emptyShadow <$> bExpr lhs
            rhs' <- emptyShadow <$> bExpr (FuncCall "*" rhs)
            tell
              [ ( this
                , JObj
                    [ ("opcode", JStr "operator_divide")
                    , ("parent", idJSON parent)
                    , ("inputs", JObj [("NUM1", lhs'), ("NUM2", rhs')])
                    ])
              ]
            pure $ NonShadow $ JStr this
        args -> throwError $ FuncWrongArgCount "/" (AtLeast 2) $ length args)
  , ( "++"
    , let go [] = bExpr $ Lit $ VStr ""
          go [x] = bExpr x
          go (lhs:rhs) =
            boil $ \this parent -> do
              lhs' <- emptyShadow <$> bExpr lhs
              rhs' <- emptyShadow <$> go rhs
              tell
                [ ( this
                  , JObj
                      [ ("opcode", JStr "operator_join")
                      , ("parent", idJSON parent)
                      , ("inputs", JObj [("STRING1", lhs'), ("STRING2", rhs')])
                      ])
                ]
              pure $ NonShadow $ JStr this
       in go)
  , ( "or"
    , let go [] = bExpr $ Lit $ VBool False
          go [x] = bExpr x
          go (lhs:rhs) =
            boil $ \this parent -> do
              lhs' <- emptyShadow <$> bExpr lhs
              rhs' <- emptyShadow <$> go rhs
              tell
                [ ( this
                  , JObj
                      [ ("opcode", JStr "operator_or")
                      , ("parent", idJSON parent)
                      , ( "inputs"
                        , JObj [("OPERAND1", lhs'), ("OPERAND2", rhs')])
                      ])
                ]
              pure $ NonShadow $ JStr this
       in go)
  , ( "and"
    , let go [] = bExpr $ Lit $ VBool True
          go [x] = bExpr x
          go (lhs:rhs) =
            boil $ \this parent -> do
              lhs' <- emptyShadow <$> bExpr lhs
              rhs' <- emptyShadow <$> go rhs
              tell
                [ ( this
                  , JObj
                      [ ("opcode", JStr "operator_and")
                      , ("parent", idJSON parent)
                      , ( "inputs"
                        , JObj [("OPERAND1", lhs'), ("OPERAND2", rhs')])
                      ])
                ]
              pure $ NonShadow $ JStr this
       in go)
  , simpleOperator "=" "operator_equals" ["OPERAND1", "OPERAND2"]
  , simpleOperator "<" "operator_lt" ["OPERAND1", "OPERAND2"]
  , simpleOperator ">" "operator_gt" ["OPERAND1", "OPERAND2"]
  , simpleOperator "str-length" "operator_length" ["STRING"]
  , simpleOperator "not" "operator_not" ["OPERAND"]
  , simpleOperator "char-at" "operator_letter_of" ["STRING", "LETTER"]
  , simpleOperator "mod" "operator_mod" ["NUM1", "NUM2"]
  , mathOp "abs" "abs"
  , mathOp "floor" "floor"
  , mathOp "ceil" "ceiling"
  , mathOp "sqrt" "sqrt"
  , mathOp "ln" "ln"
  , mathOp "log" "log"
  , mathOp "e^" "e ^"
  , mathOp "ten^" "10 ^"
  , mathOp "sin" "sin"
  , mathOp "cos" "cos"
  , mathOp "tan" "tan"
  , mathOp "asin" "asin"
  , mathOp "acos" "acos"
  , mathOp "atan" "atan"
  , ( "length"
    , \case
        [listName] ->
          boil $ \this parent -> do
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
            pure $ NonShadow $ JStr this
        args ->
          throwError $ FuncWrongArgCount "length" (Exactly 1) $ length args)
  ]
  where
    simpleOperator ::
         T.Text -> T.Text -> [T.Text] -> (T.Text, [Expr] -> Blocky Reporter)
    simpleOperator name opcode inputs = (name, go)
      where
        go args
          | length args /= length inputs =
            throwError $
            FuncWrongArgCount name (Exactly $ length inputs) $ length args
          | otherwise =
            boil $ \this parent -> do
              args' <- fmap emptyShadow <$> traverse bExpr args
              tell
                [ ( this
                  , JObj
                      [ ("opcode", JStr opcode)
                      , ("parent", idJSON parent)
                      , ("inputs", JObj $ zip inputs args')
                      ])
                ]
              pure $ NonShadow $ JStr this
    mathOp :: T.Text -> T.Text -> (T.Text, [Expr] -> Blocky Reporter)
    mathOp name op =
      ( name
      , \case
          [num] ->
            boil $ \this parent -> do
              num' <- emptyShadow <$> bExpr num
              tell
                [ ( this
                  , JObj
                      [ ("opcode", JStr "operator_mathop")
                      , ("parent", idJSON parent)
                      , ("inputs", JObj [("NUM", num')])
                      , ("fields", JObj [("OPERATOR", JArr [JStr op, JNull])])
                      ])
                ]
              pure $ NonShadow $ JStr this
          args -> throwError $ FuncWrongArgCount name (Exactly 1) (length args))

builtinSymbols :: [(T.Text, Blocky Reporter)]
builtinSymbols =
  [ ("x-pos", simpleSymbol "motion_xposition")
  , ("y-pos", simpleSymbol "motion_yposition")
  , ("timer", simpleSymbol "sensing_timer")
  ]
  where
    simpleSymbol :: T.Text -> Blocky Reporter
    simpleSymbol opcode =
      boil $ \this parent -> do
        tell [(this, JObj [("opcode", JStr opcode), ("parent", idJSON parent)])]
        pure $ NonShadow $ JStr this

withLocals :: [T.Text] -> [T.Text] -> Blocky a -> Blocky a
withLocals vars lists comp = do
  vars' <- for vars $ \v -> newID <&> \i -> (v, (i, mangle v i))
  lists' <- for lists $ \v -> newID <&> \i -> (v, (i, mangle v i))
  local (\env -> env {_envLocalVars = vars', _envLocalLists = lists'}) comp
  where
    mangle v i = "local " <> i <> " " <> v

-- Boilerplate to provide `this` and `parent` to a blocky computation
boil :: (UID -> Maybe UID -> Blocky a) -> Blocky a
boil computation = do
  this <- newID
  parent <- asks _envParent
  withParent (Just this) $ computation this parent
