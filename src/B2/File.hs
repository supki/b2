{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module B2.File
  ( File(..)
  ) where

import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import           Data.Int (Int64)
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)

import           B2.AccountID (AccountID)
import           B2.Bucket (BucketID)


data File = File
  { fileID          :: Text
  , fileName        :: Text
  , accountID       :: AccountID
  , bucketID        :: BucketID
  , contentLength   :: Int64
  , contentSha1     :: Text
  , contentType     :: Text
  , fileInfo        :: HashMap Text Text
  , uploadTimestamp :: Int64
  } deriving (Show, Eq)

instance Aeson.FromJSON File where
  parseJSON =
    Aeson.withObject "File" $ \o -> do
      fileID <- o .: "fileId"
      fileName <- o .: "fileName"
      accountID <- o .: "accountId"
      bucketID <- o .: "bucketId"
      contentLength <- o .: "contentLength"
      contentSha1 <- o .: "contentSha1"
      contentType <- o .: "contentType"
      fileInfo <- o .: "fileInfo"
      uploadTimestamp <- o .: "uploadTimestamp"
      pure File {..}
