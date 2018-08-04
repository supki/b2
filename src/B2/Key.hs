{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
module B2.Key
  ( Key(..)
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

import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Int (Int64)
import           Data.String (IsString)
import           Data.Text (Text)
import           Prelude hiding (all)

import           B2.Bucket (Bucket)
import           B2.ID (ID, Account)


data Key secret = Key
  { applicationKeyID      :: ID Key
  , applicationKey        :: secret
  , capabilities          :: [Capability]
  , accountID             :: ID Account
  , bucketID              :: Maybe (ID Bucket)
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

instance Aeson.ToJSON secret => Aeson.ToJSON (Key secret) where
  toJSON Key {..} =
    Aeson.object
      [ "applicationKeyId" .= applicationKeyID
      , "capabilities" .= capabilities
      , "accountId" .= accountID
      , "bucketId" .= bucketID
      , "expirationTimestamp" .= expirationTimestampMS
      , "keyName" .= keyName
      , "namePrefix" .= namePrefix
      ]

class HasKeyID t where
  getKeyID :: t -> ID Key

instance key ~ Key => HasKeyID (ID key) where
  getKeyID x = x

instance HasKeyID (Key secret) where
  getKeyID = applicationKeyID

newtype ApplicationKey = ApplicationKey { unApplicationKey :: Text }
    deriving         (Eq, IsString, Aeson.FromJSON, Aeson.ToJSON)
    deriving newtype (Show)

data NoSecret = NoSecret
    deriving (Show, Eq)

instance Aeson.ToJSON NoSecret where
  toJSON _ =
    Aeson.Null

newtype Capability = Capability { unCapability :: Text }
    deriving         (Eq, IsString, Aeson.FromJSON, Aeson.ToJSON)
    deriving newtype (Show)

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
  , nextApplicationKeyID :: Maybe (ID Key)
  } deriving (Show, Eq)

instance Aeson.FromJSON Keys where
  parseJSON =
    Aeson.withObject "Keys" $ \o -> do
      keys <- o .: "keys"
      nextApplicationKeyID <- o .: "nextApplicationKeyId"
      pure Keys {..}

instance Aeson.ToJSON Keys where
  toJSON Keys {..} =
    Aeson.object
      [ "keys" .= keys
      , "nextApplicationKeyId" .= nextApplicationKeyID
      ]
