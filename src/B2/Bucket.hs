{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
module B2.Bucket
  ( Bucket(..)
  , HasBucketID(..)
  , BucketType(..)
  , LifecycleRule(..)
  , CorsRule(..)
  , Buckets(..)
  ) where

import           Control.Applicative (empty)
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Int (Int64)
import           Data.HashMap.Strict (HashMap)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)

import           B2.ID (ID(..))


data Bucket = Bucket
  { accountID      :: Text
  , bucketID       :: ID Bucket
  , bucketName     :: Text
  , bucketType     :: BucketType
  , bucketInfo     :: HashMap Text Text
  , lifecycleRules :: [LifecycleRule]
  , revision       :: Int64
  } deriving (Show, Eq)

instance Aeson.FromJSON Bucket where
  parseJSON =
    Aeson.withObject "Bucket" $ \o -> do
      accountID <- o .: "accountId"
      bucketID <- o .: "bucketId"
      bucketName <- o .: "bucketName"
      bucketType <- o .: "bucketType"
      bucketInfo <- o .: "bucketInfo"
      lifecycleRules <- o .: "lifecycleRules"
      revision <- o .: "revision"
      pure Bucket {..}

instance Aeson.ToJSON Bucket where
  toJSON Bucket {..} =
    Aeson.object
      [ "accountId" .= accountID
      , "bucketId" .= bucketID
      , "bucketName" .= bucketName
      , "bucketType" .= bucketType
      , "bucketInfo" .= bucketInfo
      , "lifecycleRules" .= lifecycleRules
      , "revision" .= revision
      ]

class HasBucketID t where
  getBucketID :: t -> ID Bucket

instance bucket ~ Bucket => HasBucketID (ID bucket) where
  getBucketID x = x

instance HasBucketID Bucket where
  getBucketID = bucketID

data BucketType
  = AllPrivate
  | AllPublic
  | Snapshot
    deriving (Show, Eq)

instance Aeson.ToJSON BucketType where
  toJSON = \case
    AllPrivate -> "allPrivate"
    AllPublic -> "allPublic"
    Snapshot -> "snapshot"

instance Aeson.FromJSON BucketType where
  parseJSON =
    Aeson.withText "BucketType" $ \case
      "allPrivate" ->
        pure AllPrivate
      "allPublic" ->
        pure AllPublic
      "snapshot" ->
        pure Snapshot
      _ ->
        empty

data LifecycleRule = LifecycleRule
  { daysFromUploadingToHiding :: Maybe Int64
  , daysFromHidingToDeleting  :: Maybe Int64
  , fileNamePrefix            :: Maybe Text
  } deriving (Show, Eq)

instance Aeson.FromJSON LifecycleRule where
  parseJSON =
    Aeson.withObject "LifecycleRule" $ \o -> do
      daysFromUploadingToHiding <- o .: "daysFromUploadingToHiding"
      daysFromHidingToDeleting <- o .: "daysFromHidingToDeleting"
      fileNamePrefix <- o .: "fileNamePrefix"
      pure LifecycleRule {..}

instance Aeson.ToJSON LifecycleRule where
  toJSON LifecycleRule {..} =
    Aeson.object
      [ "daysFromUploadingToHiding" .= daysFromUploadingToHiding
      , "daysFromHidingToDeleting" .= daysFromHidingToDeleting
      , "fileNamePrefix" .= fileNamePrefix
      ]

data CorsRule = CorsRule
  { corsRuleName      :: Text
  , allowedOrigins    :: NonEmpty Text
  , allowedOperations :: NonEmpty Text
  , allowedHeaders    :: Maybe [Text]
  , exposeHeaders     :: Maybe [Text]
  , maxAgeSecods      :: Int64
  } deriving (Show, Eq)

instance Aeson.ToJSON CorsRule where
  toJSON CorsRule {..} =
    Aeson.object
      [ "corsRuleName" .= corsRuleName
      , "allowedOrigins" .= allowedOrigins
      , "allowedOperations" .= allowedOperations
      , "allowedHeaders" .= allowedHeaders
      , "exposeHeaders" .= exposeHeaders
      , "maxAgeSecods" .= maxAgeSecods
      ]

newtype Buckets = Buckets { unBuckets :: [Bucket] }
    deriving (Show, Eq)

instance Aeson.FromJSON Buckets where
  parseJSON =
    Aeson.withObject "Buckets" $ \o -> do
      unBuckets <- o .: "buckets"
      pure Buckets {..}
