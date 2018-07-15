{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module B2.Upload
  ( UploadInfo(..)
  , UploadUrl(..)
  , HasUploadUrl(..)
  , UploadPartInfo(..)
  , UploadPartUrl(..)
  , HasUploadPartUrl(..)
  ) where

import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import           Data.String (IsString)

import           B2.AuthorizationToken (AuthorizationToken, HasAuthorizationToken(..))
import           B2.Bucket (Bucket)
import           B2.File (File)
import           B2.ID (ID)


data UploadInfo = UploadInfo
  { bucketID           :: ID Bucket
  , uploadUrl          :: UploadUrl
  , authorizationToken :: AuthorizationToken
  } deriving (Show, Eq)

instance Aeson.FromJSON UploadInfo where
  parseJSON =
    Aeson.withObject "UploadInfo" $ \o -> do
      bucketID <- o .: "bucketId"
      uploadUrl <- o .: "uploadUrl"
      authorizationToken <- o .: "authorizationToken"
      pure UploadInfo {..}

instance HasAuthorizationToken UploadInfo where
  getAuthorizationToken = authorizationToken

newtype UploadUrl = UploadUrl { unUploadUrl :: String }
    deriving (Show, Eq, IsString, Aeson.FromJSON)

class HasUploadUrl t where
  getUploadUrl :: t -> UploadUrl

instance HasUploadUrl UploadUrl where
  getUploadUrl x = x

instance HasUploadUrl UploadInfo where
  getUploadUrl = uploadUrl

data UploadPartInfo = UploadPartInfo
  { fileID             :: ID File
  , uploadUrl          :: UploadPartUrl
  , authorizationToken :: AuthorizationToken
  } deriving (Show, Eq)

instance Aeson.FromJSON UploadPartInfo where
  parseJSON =
    Aeson.withObject "UploadPartInfo" $ \o -> do
      fileID <- o .: "fileId"
      uploadUrl <- o .: "uploadUrl"
      authorizationToken <- o .: "authorizationToken"
      pure UploadPartInfo {..}

instance HasAuthorizationToken UploadPartInfo where
  getAuthorizationToken = authorizationToken

newtype UploadPartUrl = UploadPartUrl { unUploadPartUrl :: String }
    deriving (Show, Eq, IsString, Aeson.FromJSON)

class HasUploadPartUrl t where
  getUploadPartUrl :: t -> UploadPartUrl

instance HasUploadPartUrl UploadPartUrl where
  getUploadPartUrl x = x

instance HasUploadPartUrl UploadPartInfo where
  getUploadPartUrl = uploadUrl
