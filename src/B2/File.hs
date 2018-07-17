{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
module B2.File
  ( File(..)
  , FileIDs(..)
  , HasFileID(..)
  , FileName(..)
  , HasFileName(..)
  , Files(..)
  ) where

import           Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import           Data.Int (Int64)
import           Data.HashMap.Strict (HashMap)
import           Data.String (IsString)
import           Data.Text (Text)

import           B2.ID (ID)


data File = File
  { fileIDs         :: FileIDs
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
      contentLength <- o .: "contentLength"
      contentSha1 <- o .: "contentSha1"
      contentType <- o .: "contentType"
      fileInfo <- o .: "fileInfo"
      action <- o .: "action"
      uploadTimestamp <- o .: "uploadTimestamp"
      pure File {..}

data FileIDs = FileIDs
  { fileID   :: ID File
  , fileName :: FileName
  } deriving (Show, Eq)

instance Aeson.FromJSON FileIDs where
  parseJSON =
    Aeson.withObject "FileIDs" $ \o -> do
      fileID <- o .: "fileId"
      fileName <- o .: "fileName"
      pure FileIDs {..}

class HasFileID t where
  getFileID :: t -> ID File

instance file ~ File => HasFileID (ID file) where
  getFileID x = x

instance HasFileID FileIDs where
  getFileID = fileID

instance HasFileID File where
  getFileID File {..} = getFileID fileIDs

newtype FileName = FileName { unFileName :: Text }
    deriving (Show, Eq, IsString, Aeson.FromJSON, Aeson.ToJSON)

class HasFileName t where
  getFileName :: t -> FileName

instance HasFileName FileName where
  getFileName x = x

instance HasFileName FileIDs where
  getFileName = fileName

instance HasFileName File where
  getFileName File {..} = getFileName fileIDs

data Files = Files
  { files        :: [File]
  , nextFileName :: Maybe Text
  , nextFileId   :: Maybe (ID File)
  } deriving (Show, Eq)

instance Aeson.FromJSON Files where
  parseJSON =
    Aeson.withObject "Files" $ \o -> do
      files <- o .: "files"
      nextFileName <- o .: "nextFileName"
      nextFileId <- o .:? "nextFileId"
      pure Files {..}
