{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Block
  ( BlockError
  , procToBlocks
  ) where

import Block.Env (Env(..), withNext, withParent, withProcArgs)
import Block.Error (ArgCount(..), BlockError(..))
import Control.Applicative ((<|>))
import Control.Monad (guard, unless)
import Control.Monad.Except (Except, throwError)
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Reader (ReaderT, ask, asks, local)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (tell)
import Data.Foldable (fold)
import Data.Functor (($>), (<&>))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Data.Traversable (for)
import JSON (JValue(..), showJStr)
import Mid.Expr (Expr(..), Value(..), toString)
import Mid.Proc (Procedure(..), Statement(..))
import UID (UID, UIDState, idJSON, newID)
import Utils.Text (arrayify)
import Utils.Trans (orThrow)

type Blocky = RWST Env [(UID, JValue)] UIDState (Except BlockError)

data Reporter =
  Rep Shape JValue

data Shape
  = Shadow
  | NonShadow

noShadow :: Reporter -> JValue
noShadow (Rep Shadow jv) = JArr [JNum 1, jv]
noShadow (Rep NonShadow jv) = JArr [JNum 2, jv]

withShadow :: JValue -> Reporter -> JValue
withShadow _ (Rep Shadow jv) = JArr [JNum 1, jv]
withShadow obscured (Rep NonShadow jv) = JArr [JNum 3, jv, obscured]

emptyShadow :: Reporter -> JValue
emptyShadow = withShadow $ JArr [JNum 10, JStr ""]

data InputFields =
  InputFields
    { _inputs :: [(T.Text, Blocky JValue)]
    , _fields :: [(T.Text, Blocky JValue)]
    }

instance Semigroup InputFields where
  InputFields i f <> InputFields i' f' = InputFields (i <> i') (f <> f')

instance Monoid InputFields where
  mempty = InputFields [] []

buildReporterOf :: Shape -> T.Text -> InputFields -> Blocky Reporter
buildReporterOf shape opcode (InputFields inputs fields) = do
  this <- newID
  parent <- asks envParent
  withParent (Just this) $ do
    inputs' <- traverse sequenceA inputs
    fields' <- traverse sequenceA fields
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr opcode)
            , ("parent", idJSON parent)
            , ("inputs", JObj inputs')
            , ("fields", JObj fields')
            ])
      ]
    pure $ Rep shape $ JStr this

buildNonShadow :: T.Text -> InputFields -> Blocky Reporter
buildNonShadow = buildReporterOf NonShadow

buildShadow :: T.Text -> InputFields -> Blocky Reporter
buildShadow = buildReporterOf Shadow

buildStacking :: T.Text -> InputFields -> Blocky (Maybe UID, Maybe UID)
buildStacking opcode (InputFields inputs fields) = do
  this <- newID
  next <- asks envNext
  parent <- asks envParent
  withParent (Just this) $ do
    inputs' <- traverse sequenceA inputs
    fields' <- traverse sequenceA fields
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr opcode)
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ("inputs", JObj inputs')
            , ("fields", JObj fields')
            ])
      ]
    pure (Just this, Just this)

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
    (bodyID, _) <- withLocals vars lists $ bStmt body
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
    (bodyID, _) <- withLocals vars lists $ bStmt body
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
    (bodyID, _) <- withLocals vars lists $ bStmt body
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
      (bodyID, _) <- bStmt body
      exisitingProcs <- asks envProcs
      protoypeID <- newID
      let paramIDs = snd <$> fromJust (lookup name exisitingProcs)
      reporters <-
        fmap noShadow <$> withParent (Just protoypeID) (traverse bExpr params)
      let argumentids = arrayify $ showJStr <$> paramIDs
          argumentnames = arrayify $ showJStr <$> params'
          argumentdefaults = arrayify $ "\"\"" <$ params
          proccode = name <> T.replicate (length params) " %s"
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
bStmt (Do xs) = bStmts xs
bStmt (IfElse cond true false) =
  boil $ \this parent -> do
    condition <- noShadow <$> bExpr cond
    true' <- bSubstack true
    (false', _) <- withNext Nothing $ bStmt false
    next <- asks envNext
    let (opcode, substack2) =
          case false' of
            Just false'' ->
              ("control_if_else", [("SUBSTACK2", JArr [JNum 2, JStr false''])])
            Nothing -> ("control_if", [])
    tell
      [ ( this
        , JObj
            [ ("opcode", JStr opcode)
            , ("next", idJSON next)
            , ("parent", idJSON parent)
            , ( "inputs"
              , JObj $
                [("CONDITION", condition), ("SUBSTACK", true')] <> substack2)
            ])
      ]
    pure (Just this, Just this)
bStmt (Repeat times body) =
  let times' = emptyShadow <$> bExpr times
      body' = bSubstack body
   in buildStacking "control_repeat" $
      InputFields [("TIMES", times'), ("SUBSTACK", body')] []
bStmt (Forever body) =
  let body' = bSubstack body
   in buildStacking "control_forever" $ InputFields [("SUBSTACK", body')] []
bStmt (Until cond body) =
  let condition = noShadow <$> bExpr cond
      body' = bSubstack body
   in buildStacking "control_repeat_until" $
      InputFields [("CONDITION", condition), ("SUBSTACK", body')] []
bStmt (While cond body) =
  let condition = noShadow <$> bExpr cond
      body' = bSubstack body
   in buildStacking "control_while" $
      InputFields [("CONDITION", condition), ("SUBSTACK", body')] []
bStmt (For var times body) =
  let var' =
        case var of
          Sym sym -> varField sym
          _ -> throwError $ InvalidArgsForBuiltinProc "for"
      times' = emptyShadow <$> bExpr times
      body' = bSubstack body
   in buildStacking "control_for_each" $
      InputFields [("VALUE", times'), ("SUBSTACK", body')] [("VARIABLE", var')]
bStmt (ProcCall name args) =
  case lookup name builtinProcs of
    Just fn -> fn args
    Nothing -> callCustomProc name args

callCustomProc :: T.Text -> [Expr] -> Blocky (Maybe UID, Maybe UID)
callCustomProc name args =
  boil $ \this parent -> do
    exisitingProcs <- asks envProcs
    paramIDs <-
      orThrow (UnknownProc name) $ fmap snd <$> lookup name exisitingProcs
    let nparams = length paramIDs
        nargs = length args
    unless (nparams == nargs) $
      throwError $ ProcWrongArgCount name (Exactly nparams) nargs
    next <- asks envNext
    args' <- fmap emptyShadow <$> traverse bExpr args
    let inputs = zip paramIDs args'
        proccode = name <> T.replicate (length args) " %s"
        argumentids = arrayify $ showJStr <$> paramIDs
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

builtinProcs :: [(T.Text, [Expr] -> Blocky (Maybe UID, Maybe UID))]
builtinProcs =
  ((\(name, opcode, inputs) -> (name, stackBlock opcode inputs name)) <$>
   [ ("erase-all", "pen_clear", [])
   , ("stamp", "pen_stamp", [])
   , ("pen-down", "pen_penDown", [])
   , ("pen-up", "pen_penUp", [])
   , ("set-xy", "motion_gotoxy", [num "X", num "Y"])
   , ("set-size", "looks_setsizeto", [num "SIZE"])
   , ("set-costume", "looks_switchcostumeto", [string "COSTUME"])
   , ("show", "looks_show", [])
   , ("hide", "looks_hide", [])
   , ("say", "looks_say", [string "MESSAGE"])
   , ("change-x", "motion_changexby", [num "DX"])
   , ("change-y", "motion_changeyby", [num "DY"])
   , ("set-x", "motion_setx", [num "X"])
   , ("set-y", "motion_sety", [num "Y"])
   , ("wait", "control_wait", [num "DURATION"])
   , ("ask", "sensing_askandwait", [string "QUESTION"])
   , ( "send-broadcast-sync"
     , "event_broadcastandwait"
     , [ \name ->
           let name' =
                 case name of
                   Lit lit ->
                     pure $
                     JArr [JNum 1, JArr [JNum 11, JStr (toString lit), JStr ""]]
                   n -> noShadow <$> bExpr n
            in InputFields [("BROADCAST_INPUT", name')] []
       ])
   , (":=", "data_setvariableto", [variable "VARIABLE" ":=", string "VALUE"])
   , ("+=", "data_changevariableby", [variable "VARIABLE" "+=", string "VALUE"])
   , ( "replace"
     , "data_replaceitemoflist"
     , [list "LIST" "replace", num "INDEX", string "ITEM"])
   , ("append", "data_addtolist", [list "LIST" "append", string "ITEM"])
   , ("delete", "data_deleteoflist", [list "LIST" "delete", num "INDEX"])
   , ("delete-all", "data_deletealloflist", [list "LIST" "delete-all"])
   ]) <>
  [ nullary "stop-all" "control_stop" $
    InputFields [] [("STOP_OPTION", pure $ JArr [JStr "all", JNull])]
  , nullary "stop-this-script" "control_stop" $
    InputFields [] [("STOP_OPTION", pure $ JArr [JStr "this script", JNull])]
  , nullary "stop-other-scripts" "control_stop" $
    InputFields
      []
      [("STOP_OPTION", pure $ JArr [JStr "other scripts in sprite", JNull])]
  , let menu =
          buildShadow "control_create_clone_of_menu" $
          InputFields
            []
            [("CLONE_OPTION", pure $ JArr [JStr "_myself_", JNull])]
     in nullary "clone-myself" "control_create_clone_of" $
        InputFields [("CLONE_OPTION", noShadow <$> menu)] []
  ]
  where
    string name expr = InputFields [(name, emptyShadow <$> bExpr expr)] []
    num = string
    variable name procName expr =
      InputFields
        []
        [ ( name
          , case expr of
              Sym varName -> varField varName
              _ -> throwError $ InvalidArgsForBuiltinProc procName)
        ]
    list name procName expr =
      InputFields
        []
        [ ( name
          , case expr of
              Sym listName -> listField listName
              _ -> throwError $ InvalidArgsForBuiltinProc procName)
        ]
    nullary name opcode inputs =
      ( name
      , \case
          [] -> buildStacking opcode inputs
          _ -> throwError $ InvalidArgsForBuiltinProc name)

varField :: T.Text -> Blocky JValue
varField name = do
  vars <- asks $ envLocalVars <> envSpriteVars <> envGlobalVars
  case lookup name vars of
    Just (varID, name') -> pure $ JArr [JStr name', JStr varID]
    Nothing -> throwError $ VarDoesntExist name

listField :: T.Text -> Blocky JValue
listField name = do
  lists <- asks $ envLocalLists <> envSpriteLists <> envGlobalLists
  case lookup name lists of
    Just (listID, name') -> pure $ JArr [JStr name', JStr listID]
    Nothing -> throwError $ ListDoesntExist name

stackBlock ::
     T.Text
  -> [Expr -> InputFields]
  -> T.Text
  -> [Expr]
  -> Blocky (Maybe UID, Maybe UID)
stackBlock opcode inputs procName args
  | length args /= length inputs =
    throwError $ InvalidArgsForBuiltinProc procName
  | otherwise = buildStacking opcode $ fold $ zipWith ($) inputs args

bStmts :: [Statement] -> Blocky (Maybe UID, Maybe UID)
bStmts [] = asks $ (Nothing, ) . envParent
bStmts [x] = bStmt x
bStmts (x:xs) =
  mdo (firstStart, firstEnd) <- withNext restStart $ bStmt x
      (restStart, restEnd) <- withParent firstEnd $ bStmts xs
      pure (firstStart, restEnd)

bExpr :: Expr -> Blocky Reporter
bExpr (Lit lit) = pure $ Rep Shadow $ JArr [JNum 10, JStr $ toString lit]
bExpr (Sym sym) = do
  env <- ask
  let procArgs = envProcArgs env
      vars = envLocalVars <> envSpriteVars <> envGlobalVars $ env
      lists = envLocalLists <> envSpriteLists <> envGlobalLists $ env
      theProcArg =
        guard (sym `elem` procArgs) $>
        buildNonShadow
          "argument_reporter_string_number"
          (InputFields [] [("VALUE", pure $ JArr [JStr sym, JNull])])
      theVar =
        lookup sym vars <&> \(i, name) ->
          pure $ Rep NonShadow $ JArr [JNum 12, JStr name, JStr i]
      theList =
        lookup sym lists <&> \(i, name) ->
          pure $ Rep NonShadow $ JArr [JNum 13, JStr name, JStr i]
      theBuiltin = lookup sym builtinSymbols
  fromMaybe (throwError $ UnknownSymbolInExpr sym) $
    theProcArg <|> theVar <|> theList <|> theBuiltin
bExpr (FuncCall name args) =
  maybe (throwError $ UnknownFunc name) ($ args) $ lookup name builtinFuncs

builtinFuncs :: [(T.Text, [Expr] -> Blocky Reporter)]
builtinFuncs =
  [ ( "!!"
    , \case
        [list, index] ->
          let list' =
                case list of
                  Sym sym -> listField sym
                  _ -> throwError $ InvalidArgsForBuiltinFunc "!!"
              index' = emptyShadow <$> bExpr index
           in buildNonShadow "data_itemoflist" $
              InputFields [("INDEX", index')] [("LIST", list')]
        args -> throwError $ FuncWrongArgCount "!!" (Exactly 2) $ length args)
  , ("+", associative "operator_add" "NUM1" "NUM2" emptyShadow $ Lit $ VNum 0)
  , ( "-"
    , \case
        [] -> throwError $ FuncWrongArgCount "-" (AtLeast 1) 0
        [x] -> bExpr (FuncCall "-" [Lit (VNum 0), x])
        (lhs:rhs) ->
          let lhs' = emptyShadow <$> bExpr lhs
              rhs' = emptyShadow <$> bExpr (FuncCall "+" rhs)
           in buildNonShadow "operator_subtract" $
              InputFields [("NUM1", lhs'), ("NUM2", rhs')] [])
  , ( "*"
    , associative "operator_multiply" "NUM1" "NUM2" emptyShadow $ Lit $ VNum 1)
  , ( "/"
    , \case
        (lhs:rhs@(_:_)) ->
          let lhs' = emptyShadow <$> bExpr lhs
              rhs' = emptyShadow <$> bExpr (FuncCall "*" rhs)
           in buildNonShadow "operator_divide" $
              InputFields [("NUM1", lhs'), ("NUM2", rhs')] []
        args -> throwError $ FuncWrongArgCount "/" (AtLeast 2) $ length args)
  , ( "++"
    , associative "operator_join" "STRING1" "STRING2" emptyShadow $
      Lit $ VStr "")
  , ( "or"
    , associative "operator_or" "OPERAND1" "OPERAND2" noShadow $
      Lit $ VBool False)
  , ( "and"
    , associative "operator_and" "OPERAND1" "OPERAND2" noShadow $
      Lit $ VBool True)
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
          let list' =
                case listName of
                  Sym sym -> listField sym
                  _ -> throwError $ InvalidArgsForBuiltinFunc "length"
           in buildNonShadow "data_lengthoflist" $
              InputFields [] [("LIST", list')]
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
            let args' = fmap emptyShadow . bExpr <$> args
             in buildNonShadow opcode $ InputFields (zip inputs args') []
    mathOp :: T.Text -> T.Text -> (T.Text, [Expr] -> Blocky Reporter)
    mathOp name op =
      ( name
      , \case
          [num] ->
            let num' = emptyShadow <$> bExpr num
             in buildNonShadow "operator_mathop" $
                InputFields
                  [("NUM", num')]
                  [("OPERATOR", pure $ JArr [JStr op, JNull])]
          args -> throwError $ FuncWrongArgCount name (Exactly 1) (length args))
    associative opcode lhsName rhsName shadow zero =
      let go [] = bExpr zero
          go [x] = bExpr x
          go (lhs:rhs) =
            let lhs' = shadow <$> bExpr lhs
                rhs' = shadow <$> go rhs
             in buildNonShadow opcode $
                InputFields [(lhsName, lhs'), (rhsName, rhs')] []
       in go

bSubstack :: Statement -> Blocky JValue
bSubstack = fmap (\(i, _) -> JArr [JNum 2, idJSON i]) . withNext Nothing . bStmt

builtinSymbols :: [(T.Text, Blocky Reporter)]
builtinSymbols =
  fmap simpleSymbol <$>
  [ ("x-pos", "motion_xposition")
  , ("y-pos", "motion_yposition")
  , ("timer", "sensing_timer")
  , ("answer", "sensing_answer")
  ]
  where
    simpleSymbol opcode = buildNonShadow opcode $ InputFields [] []

withLocals :: [T.Text] -> [T.Text] -> Blocky a -> Blocky a
withLocals vars lists comp = do
  vars' <- for vars $ \v -> newID <&> \i -> (v, (i, mangle v i))
  lists' <- for lists $ \v -> newID <&> \i -> (v, (i, mangle v i))
  local (\env -> env {envLocalVars = vars', envLocalLists = lists'}) comp
  where
    mangle v i = "local " <> i <> " " <> v

-- Boilerplate to provide `this` and `parent` to a blocky computation
boil :: (UID -> Maybe UID -> Blocky a) -> Blocky a
boil computation = do
  this <- newID
  parent <- asks envParent
  withParent (Just this) $ computation this parent
