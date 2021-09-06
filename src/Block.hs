{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Block
  ( Env(..)
  , BlockError
  , procToBlocks
  ) where

import Control.Monad (unless)
import Control.Monad.Except (Except, throwError)
import Control.Monad.RWS (RWST, evalRWST)
import Control.Monad.Reader (asks, local)
import Control.Monad.Writer (tell)
import qualified Data.Text as T
import JSON (JValue(..))
import Lens.Micro (Lens', set)
import Mid.Expr (Expr(..), Value(..))
import Mid.Proc (Procedure(..), Statement(..))
import Text.Printf (printf)
import UID (UID, UIDState, idJSON, newID, prependID)

data BlockError
  = InvalidParamsForSpecialProcDef T.Text
  | UnknownProc T.Text

instance Show BlockError where
  show (InvalidParamsForSpecialProcDef procName) =
    printf "invalid arguments for definition of special procedure `%s`" procName
  show (UnknownProc procName) = printf "unknown procedure `%s`" procName

data Env =
  Env
    { _envParent :: Maybe UID
    , _envNext :: Maybe UID
    , _envProcs :: [T.Text]
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
procToBlocks env proc = snd <$> evalRWST (bProc proc) env ([], 0)

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
bStmt (ProcCall procName args) = do
  exisitingProcs <- asks _envProcs
  unless (procName `elem` exisitingProcs) $ throwError $ UnknownProc procName
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
            , ("CONDITION", condition)
            , ("SUBSTACK", idJSON trueID)
            , ("SUBSTACK2", idJSON falseID)
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
            , ("TIMES", times')
            , ("SUBSTACK", idJSON bodyID)
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
            , ("SUBSTACK", idJSON bodyID)
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
            , ("CODITION", condition)
            , ("SUBSTACK", idJSON bodyID)
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
            , ("CODITION", condition)
            , ("SUBSTACK", idJSON bodyID)
            ])
      ]
    return (Just this, Just this)
bStmt (For var times body) = undefined

bStmts :: [Statement] -> Blocky (Maybe UID, Maybe UID)
bStmts [] = asks $ (Nothing, ) <$> _envParent
bStmts (x:xs) = do
  next <- newID
  (firstStart, firstEnd) <- withNext (Just next) $ bStmt x
  prependID next
  (_, restEnd) <- withParent firstEnd $ bStmts xs
  return (firstStart, restEnd)

bExpr :: Expr -> Blocky JValue
bExpr (Lit (VNum num)) = return $ JArr [JNum 4, JNum num]
bExpr (Lit (VStr str)) = return $ JArr [JNum 10, JStr str]
bExpr (Lit (VBool True)) = return $ JArr [JNum 10, JStr "true"]
bExpr (Lit (VBool False)) = return $ JArr [JNum 10, JStr "false"]
bExpr (Sym sym) = undefined
bExpr (FuncCall name args) = undefined
