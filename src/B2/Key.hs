{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module B2.Key
  ( Key(..)
  , KeyID(..)
  , HasKeyID(..)
  , ApplicationKey(..)
  , NoSecret(..)
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


data Key secret = Key
  { applicationKeyID      :: KeyID
  , applicationKey        :: secret
  , capabilities          :: [Capability]
  , accountID             :: AccountID
  , bucketID              :: Maybe BucketID
  , expirationTimestampMS :: Maybe Int64
  , keyName               :: Text
  , namePrefix            :: Maybe Text
  } deriving (Show, Eq)

instance Aeson.FromJSON (Key ApplicationKey) where
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

instance Aeson.FromJSON (Key NoSecret) where
  parseJSON =
    Aeson.withObject "Key" $ \o -> do
      applicationKeyID <- o .: "applicationKeyId"
      capabilities <- o .: "capabilities"
      accountID <- o .: "accountId"
      bucketID <- o .: "bucketId"
      expirationTimestampMS <- o .: "expirationTimestamp"
      keyName <- o .: "keyName"
      namePrefix <- o .: "namePrefix"
      pure Key {applicationKey=NoSecret, ..}

newtype KeyID = KeyID { unKeyID :: Text }
    deriving (Show, Eq, IsString, Aeson.FromJSON, Aeson.ToJSON)

class HasKeyID t where
  getKeyID :: t -> KeyID

instance HasKeyID KeyID where
  getKeyID x = x

instance HasKeyID (Key secret) where
  getKeyID = applicationKeyID

newtype ApplicationKey = ApplicationKey { unApplicationKey :: Text }
    deriving (Show, Eq, IsString, Aeson.FromJSON)

data NoSecret = NoSecret
    deriving (Show, Eq)

newtype Capability = Capability { unCapability :: Text }
    deriving (Show, Eq, IsString, Aeson.FromJSON, Aeson.ToJSON)

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
  { keys                 :: [Key NoSecret]
  , nextApplicationKeyID :: Maybe KeyID
  } deriving (Show, Eq)

instance Aeson.FromJSON Keys where
  parseJSON =
    Aeson.withObject "Keys" $ \o -> do
      keys <- o .: "keys"
      nextApplicationKeyID <- o .: "nextApplicationKeyId"
      pure Keys {..}
