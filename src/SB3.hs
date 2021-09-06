{-# LANGUAGE OverloadedStrings #-}

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
import Data.Binary (encodeFile)
import Data.Traversable (for)
import JSON (JValue(..), showJSON)
import Lens.Micro ((^.), (^..))
import Lens.Micro.Extras (view)
import Mid (Program, targets)
import Mid.Proc (procedureName)
import Mid.Sprite (Sprite, costumes, isStage, procedures, spriteName)
import Utils.Trans (hoistExcept)

writeSB3File :: Program -> ExceptT BlockError IO ()
writeSB3File prg = do
  (projectObj, assets) <- projectJSON prg
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

projectJSON :: Program -> ExceptT BlockError IO (JValue, [Asset])
projectJSON prg = do
  let meta = JObj [("semver", JStr "3.0.0")]
  (targets', assetLists) <- unzip <$> traverse spriteJSON (prg ^.. targets)
  let assets = concat assetLists
  return (JObj [("meta", meta), ("targets", JArr targets')], assets)

spriteJSON :: Sprite -> ExceptT BlockError IO (JValue, [Asset])
spriteJSON spr = do
  costumes' <- liftIO $ traverse (uncurry makeAsset) $ spr ^. costumes
  let env =
        Env
          { _envParent = Nothing
          , _envNext = Nothing
          , _envProcs = view procedureName <$> spr ^. procedures
          , _envLocalVars = []
          , _envGlobalVars = []
          , _envLocalLists = []
          , _envGlobalLists = []
          }
  blocks <-
    hoistExcept $ concat <$> traverse (procToBlocks env) (spr ^. procedures)
  return
    ( JObj
        [ ("name", JStr (spr ^. spriteName))
        , ("isStage", JBool (isStage spr))
        , ("variables", JObj [])
        , ("costumes", JArr (assetJSON <$> costumes'))
        , ("currentCostume", JNum 1)
        , ("sounds", JArr [])
        , ("blocks", JObj blocks)
        ]
    , costumes')
