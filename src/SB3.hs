{-# LANGUAGE OverloadedStrings #-}

module SB3
  ( writeSB3File
  ) where

import Asset (Asset(assetPath), assetId, assetJSON, makeAsset)
import Codec.Archive.Zip
  ( Archive
  , Entry(eRelativePath)
  , addEntryToArchive
  , emptyArchive
  , readEntry
  , toEntry
  )
import Control.Arrow ((&&&))
import Data.Binary (encodeFile)
import Data.Traversable (for)
import JSON (JValue(..), showJSON)
import Lens.Micro ((^.), (^..))
import Mid (Program, targets)
import Mid.Sprite (Sprite, costumes, isStage, spriteName)

writeSB3File :: Program -> IO ()
writeSB3File prg = do
  (projectObj, assets) <- projectJSON prg
  let projectEntry = toEntry "project.json" 0 $ showJSON projectObj
  let archive' = addEntryToArchive projectEntry emptyArchive
  archive <- addFilesToArchiveAt ((assetPath &&& assetId) <$> assets) archive'
  encodeFile "project.sb3" archive

addFilesToArchiveAt :: [(FilePath, FilePath)] -> Archive -> IO Archive
addFilesToArchiveAt paths arch = foldr addEntryToArchive arch <$> entries
  where
    entries =
      for paths $ \(origPath, newPath) ->
        (\e -> e {eRelativePath = newPath}) <$> readEntry [] origPath

projectJSON :: Program -> IO (JValue, [Asset])
projectJSON prg = do
  let meta = JObj [("semver", JStr "3.0.0")]
  (targets', assetLists) <- unzip <$> traverse spriteJSON (prg ^.. targets)
  let assets = concat assetLists
  return (JObj [("meta", meta), ("targets", JArr targets')], assets)

spriteJSON :: Sprite -> IO (JValue, [Asset])
spriteJSON spr = do
  costumes' <- traverse (uncurry makeAsset) $ spr ^. costumes
  return
    ( JObj
        [ ("name", JStr (spr ^. spriteName))
        , ("isStage", JBool (isStage spr))
        , ("variables", JObj [])
        , ("costumes", JArr (assetJSON <$> costumes'))
        , ("currentCostume", JNum 1)
        , ("sounds", JArr [])
        , ("blocks", JObj [])
        ]
    , costumes')
