{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module B2.Key
  ( Key(..)
  , Capability(..)
  , all
  , listKeys
  , writeKeys
  , deleteKeys
  , listBuckets
  , writeBuckets
  , deleteBuckets
  , listFiles
  , readFiles
  , shareFiles
  , writeFiles
  , deleteFiles
  , Keys(..)
  ) where

import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import           Data.Int (Int64)
import           Data.String (IsString)
import           Data.Text (Text)
import           Prelude hiding (all)

import           B2.AccountID (AccountID)
import           B2.Bucket (BucketID)


data Key = Key
  { applicationKeyID      :: Text
  , applicationKey        :: Text
  , capabilities          :: [Capability]
  , accountID             :: AccountID
  , bucketID              :: Maybe BucketID
  , expirationTimestampMS :: Int64
  , keyName               :: Text
  , namePrefix            :: Maybe Text
  } deriving (Show, Eq)

instance Aeson.FromJSON Key where
  parseJSON =
    Aeson.withObject "Key" $ \o -> do
      applicationKeyID <- o .: "applicationKeyId"
      applicationKey <- o .: "applicationKey"
      capabilities <- o .: "capabilities"
      accountID <- o .: "accountId"
      bucketID <- o .: "bucketId"
      expirationTimestampMS <- o .: "expirationTimestamp"
      keyName <- o .: "keyName"
      namePrefix <- o .: "namePrefix"
      pure Key {..}

newtype Capability = Capability { unCapability :: Text }
    deriving (Show, Eq, IsString, Aeson.FromJSON)

all :: Capability
all = "all"

listKeys, writeKeys, deleteKeys :: Capability
listKeys = "listKeys"
writeKeys = "writeKeys"
deleteKeys = "deleteKeys"

listBuckets, writeBuckets, deleteBuckets :: Capability
listBuckets = "listBuckets"
writeBuckets = "writeBuckets"
deleteBuckets = "deleteBuckets"

listFiles, readFiles, shareFiles, writeFiles, deleteFiles :: Capability
listFiles = "listFiles"
readFiles = "readFiles"
shareFiles = "shareFiles"
writeFiles = "writeFiles"
deleteFiles = "deleteFiles"

data Keys = Keys
  { keys :: [Key]
  , nextApplicationKeyID :: Maybe Text
  } deriving (Show, Eq)

instance Aeson.FromJSON Keys where
  parseJSON =
    Aeson.withObject "Keys" $ \o -> do
      keys <- o .: "keys"
      nextApplicationKeyID <- o .: "nextApplicationKeyId"
      pure Keys {..}
