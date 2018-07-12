{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module B2.Upload
  ( UploadInfo(..)
  , UploadUrl(..)
  , HasUploadUrl(..)
  ) where

import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import           Data.String (IsString)

import           B2.AuthorizationToken (AuthorizationToken, HasAuthorizationToken(..))
import           B2.Bucket (BucketID)


data UploadInfo = UploadInfo
  { bucketID           :: BucketID
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
