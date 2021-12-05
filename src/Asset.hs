{-# LANGUAGE OverloadedStrings #-}

module Asset
  ( Asset(..)
  , makeAsset
  , assetJSON
  , assetId
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Text as T
import JSON (JValue(..))
import System.FilePath (takeExtension)

data Asset =
  Asset
    { assetName :: T.Text
    , assetPath :: FilePath
    , assetMD5 :: T.Text
    }

makeAsset :: T.Text -> FilePath -> IO Asset
makeAsset name path = Asset name path . T.pack . show . md5 <$> B.readFile path

assetJSON :: Asset -> JValue
assetJSON (Asset name path md5sum) =
  let ext = T.pack $ takeExtension path
   in JObj
        [ ("assetId", JStr md5sum)
        , ("name", JStr name)
        , ("md5ext", JStr (md5sum <> ext))
        , ("dataFormat", JStr (T.tail ext))
        ]

assetId :: Asset -> FilePath
assetId asset = T.unpack (assetMD5 asset) ++ takeExtension (assetPath asset)
