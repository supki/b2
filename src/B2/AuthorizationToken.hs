{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module B2.AuthorizationToken
  ( HasAuthorizationToken(..)
  , AuthorizationToken(..)
  , AuthorizeAccount(..)
  , Allowed(..)
  , DownloadAuthorization(..)
  ) where

import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Int (Int64)
import           Data.String (IsString)
import           Data.Text (Text)

import           B2.ID (ID, Account, HasAccountID(..))
import           B2.Bucket (Bucket)
import           B2.Key (Capability)
import           B2.Url (BaseUrl, HasBaseUrl(..), DownloadUrl, HasDownloadUrl(..))


newtype AuthorizationToken = AuthorizationToken { unAuthorizationToken :: Text }
    deriving (Show, Eq, IsString, Aeson.FromJSON, Aeson.ToJSON)

class HasAuthorizationToken t where
  getAuthorizationToken :: t -> AuthorizationToken

instance HasAuthorizationToken AuthorizationToken where
  getAuthorizationToken x = x

data DownloadAuthorization = DownloadAuthorization
  { authorizationToken :: AuthorizationToken
  , bucketID           :: ID Bucket
  , fileNamePrefix     :: Text
  } deriving (Show, Eq)

instance Aeson.FromJSON DownloadAuthorization where
  parseJSON =
    Aeson.withObject "DownloadAuthorization" $ \o -> do
      authorizationToken <- o .: "authorizationToken"
      bucketID <- o .: "bucketId"
      fileNamePrefix <- o .: "fileNamePrefix"
      pure DownloadAuthorization {..}

instance Aeson.ToJSON DownloadAuthorization where
  toJSON DownloadAuthorization {..} =
    Aeson.object
      [ "authorizationToken" .= authorizationToken
      , "bucketId" .= bucketID
      , "fileNamePrefix" .= fileNamePrefix
      ]

instance HasAuthorizationToken DownloadAuthorization where
  getAuthorizationToken = authorizationToken

data AuthorizeAccount = AuthorizeAccount
  { accountID               :: ID Account
  , authorizationToken      :: AuthorizationToken
  , allowed                 :: Allowed
  , apiUrl                  :: BaseUrl
  , downloadUrl             :: DownloadUrl
  , recommendedPartSize     :: Int64
  , absoluteMinimumPartSize :: Int64
  } deriving (Show, Eq)

instance Aeson.FromJSON AuthorizeAccount where
  parseJSON =
    Aeson.withObject "AuthorizeAccount" $ \o -> do
      accountID <- o .: "accountId"
      authorizationToken <- o .: "authorizationToken"
      allowed <- o .: "allowed"
      apiUrl <- o .: "apiUrl"
      downloadUrl <- o .: "downloadUrl"
      recommendedPartSize <- o .: "recommendedPartSize"
      absoluteMinimumPartSize <- o .: "absoluteMinimumPartSize"
      pure AuthorizeAccount {..}

instance HasAuthorizationToken AuthorizeAccount where
  getAuthorizationToken AuthorizeAccount {..} = authorizationToken

instance HasBaseUrl AuthorizeAccount where
  getBaseUrl AuthorizeAccount {..} = apiUrl

instance HasDownloadUrl AuthorizeAccount where
  getDownloadUrl AuthorizeAccount {..} = downloadUrl

data Allowed = Allowed
  { capabilities :: [Capability]
  , bucketID     :: Maybe (ID Bucket)
  , namePrefix   :: Maybe Text
  } deriving (Show, Eq)

instance Aeson.FromJSON Allowed where
  parseJSON =
    Aeson.withObject "Allowed" $ \o -> do
      capabilities <- o .: "capabilities"
      bucketID <- o .: "bucketId"
      namePrefix <- o .: "namePrefix"
      pure Allowed {..}

instance HasAccountID AuthorizeAccount where
  getAccountID AuthorizeAccount {..} = accountID
