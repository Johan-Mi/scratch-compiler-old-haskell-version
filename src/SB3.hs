{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module SB3
  ( writeSB3File
  ) where

import Asset (Asset(assetPath), assetId, assetJSON, makeAsset)
import Block (BlockError, Env(..), procToBlocks)
import Codec.Archive.Zip
  ( Archive
  , Entry(eRelativePath)
  , addEntryToArchive
  , emptyArchive
  , readEntry
  , toEntry
  )
import Control.Arrow ((&&&))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (StateT, mapStateT)
import Data.Binary (encodeFile)
import Data.Functor ((<&>))
import Data.Traversable (for)
import JSON (JValue(..), showJSON)
import Lens.Micro ((^.), (^..))
import Mid (Program, stage, targets)
import Mid.Expr (Expr(..))
import Mid.Proc (procedureName, procedureParams)
import Mid.Sprite
  ( Sprite
  , costumes
  , isStage
  , lists
  , procedures
  , spriteName
  , variables
  )
import UID (UIDState, newID, runUIDGenerator)
import Utils.Trans (hoistExcept)

writeSB3File :: Program -> ExceptT BlockError IO ()
writeSB3File prg = do
  (projectObj, assets) <- runUIDGenerator $ projectJSON prg
  let projectEntry = toEntry "project.json" 0 $ showJSON projectObj
  let archive' = addEntryToArchive projectEntry emptyArchive
  archive <-
    liftIO $ addFilesToArchiveAt ((assetPath &&& assetId) <$> assets) archive'
  liftIO $ encodeFile "project.sb3" archive

addFilesToArchiveAt :: [(FilePath, FilePath)] -> Archive -> IO Archive
addFilesToArchiveAt paths arch = foldr addEntryToArchive arch <$> entries
  where
    entries =
      for paths $ \(origPath, newPath) ->
        (\e -> e {eRelativePath = newPath}) <$> readEntry [] origPath

projectJSON ::
     Program -> StateT UIDState (ExceptT BlockError IO) (JValue, [Asset])
projectJSON prg = do
  let meta = JObj [("semver", JStr "3.0.0")]
  globalVars <- for (prg ^. stage . variables) $ \v -> (v, ) <$> newID
  globalLists <- for (prg ^. stage . lists) $ \v -> (v, ) <$> newID
  let env =
        Env
          { _envParent = Nothing
          , _envNext = Nothing
          , _envProcs = []
          , _envProcArgs = []
          , _envLocalVars = []
          , _envGlobalVars = globalVars
          , _envLocalLists = []
          , _envGlobalLists = globalLists
          }
  (targets', assetLists) <-
    unzip <$> traverse (spriteJSON env) (prg ^.. targets)
  let assets = concat assetLists
  return (JObj [("meta", meta), ("targets", JArr targets')], assets)

spriteJSON ::
     Env -> Sprite -> StateT UIDState (ExceptT BlockError IO) (JValue, [Asset])
spriteJSON env spr = do
  costumes' <- liftIO $ traverse (uncurry makeAsset) $ spr ^. costumes
  localVars <- for (spr ^. variables) $ \v -> (v, ) <$> newID
  localLists <- for (spr ^. lists) $ \v -> (v, ) <$> newID
  procs <-
    for (spr ^. procedures) $ \p -> do
      params <- for [s | Sym s <- p ^. procedureParams] $ \v -> (v, ) <$> newID
      return (p ^. procedureName, params)
  let env' =
        env
          { _envProcs = procs
          , _envLocalVars = localVars
          , _envLocalLists = localLists
          }
  blocks <-
    concat <$>
    mapStateT
      hoistExcept
      (flip runReaderT env' $ traverse procToBlocks $ spr ^. procedures)
  return
    ( JObj
        [ ("name", JStr (spr ^. spriteName))
        , ("isStage", JBool (isStage spr))
        , ( "variables"
          , JObj $ localVars <&> \(v, i) -> (i, JArr [JStr v, JNum 0]))
        , ( "lists"
          , JObj $ localLists <&> \(v, i) -> (i, JArr [JStr v, JArr []]))
        , ("costumes", JArr (assetJSON <$> costumes'))
        , ("currentCostume", JNum 1)
        , ("sounds", JArr [])
        , ("blocks", JObj blocks)
        ]
    , costumes')
