{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module B2.File
  ( File(..)
  , FileIDs(..)
  , FileID(..)
  , HasFileID(..)
  , FileName(..)
  , HasFileName(..)
  ) where

import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import           Data.Int (Int64)
import           Data.HashMap.Strict (HashMap)
import           Data.String (IsString)
import           Data.Text (Text)

import           B2.AccountID (AccountID)
import           B2.Bucket (BucketID)


data File = File
  { fileIDs         :: FileIDs
  , accountID       :: AccountID
  , bucketID        :: BucketID
  , contentLength   :: Int64
  , contentSha1     :: Text
  , contentType     :: Text
  , fileInfo        :: HashMap Text Text
  , action          :: Text
  , uploadTimestamp :: Int64
  } deriving (Show, Eq)

instance Aeson.FromJSON File where
  parseJSON =
    Aeson.withObject "File" $ \o -> do
      fileIDs <- Aeson.parseJSON (Aeson.Object o)
      accountID <- o .: "accountId"
      bucketID <- o .: "bucketId"
      contentLength <- o .: "contentLength"
      contentSha1 <- o .: "contentSha1"
      contentType <- o .: "contentType"
      fileInfo <- o .: "fileInfo"
      action <- o .: "action"
      uploadTimestamp <- o .: "uploadTimestamp"
      pure File {..}

data FileIDs = FileIDs
  { fileID   :: FileID
  , fileName :: FileName
  } deriving (Show, Eq)

instance Aeson.FromJSON FileIDs where
  parseJSON =
    Aeson.withObject "FileIDs" $ \o -> do
      fileID <- o .: "fileId"
      fileName <- o .: "fileName"
      pure FileIDs {..}

newtype FileID = FileID { unFileID :: Text }
    deriving (Show, Eq, IsString, Aeson.FromJSON, Aeson.ToJSON)

class HasFileID t where
  getFileID :: t -> FileID

instance HasFileID FileID where
  getFileID x = x

instance HasFileID FileIDs where
  getFileID = fileID

instance HasFileID File where
  getFileID = getFileID . fileIDs

newtype FileName = FileName { unFileName :: Text }
    deriving (Show, Eq, IsString, Aeson.FromJSON, Aeson.ToJSON)

class HasFileName t where
  getFileName :: t -> FileName

instance HasFileName FileName where
  getFileName x = x

instance HasFileName FileIDs where
  getFileName = fileName

instance HasFileName File where
  getFileName = getFileName . fileIDs
