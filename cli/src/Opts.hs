{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module Opts
  ( Cmd(..)
  , get
  ) where

import           Control.Monad ((<=<))
import           Options.Applicative
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Env

import qualified B2
import qualified Cfg



data Cmd
  = CreateKey
      [B2.Capability]
      Text
      Int64
      (Maybe (B2.ID B2.Bucket))
      (Maybe Text)
  | ListKeys
      (Maybe Int64)
      (Maybe (B2.ID B2.Key))
  | DeleteKey
      (B2.ID B2.Key)
  | CreateBucket
      B2.BucketType
      Text
  | ListBuckets
      (Maybe (B2.ID B2.Bucket))
      (Maybe Text)
      (Maybe [B2.BucketType])
  | DeleteBucket
      (B2.ID B2.Bucket)
  | UploadFile
      (B2.ID B2.Bucket)
      Text
      FilePath
      (Maybe Text)
  | ListFileNames
      (B2.ID B2.Bucket)
      (Maybe Text)
      (Maybe Int64)
      (Maybe Text)
      (Maybe Char)
  | ListFileVersions
      (B2.ID B2.Bucket)
      (Maybe Text)
      (Maybe (B2.ID B2.File))
      (Maybe Int64)
      (Maybe Text)
      (Maybe Char)
  | DownloadById
      (B2.ID B2.File)
      FilePath
      (Maybe Int64)
      (Maybe Int64)
    deriving (Show, Eq)

get :: IO Cmd
get =
  customExecParser parserPrefs (info (helper <*> parser) (fullDesc <> header Cfg.usageHeader))
 where
  parserPrefs =
    prefs showHelpOnError
  parser =
    subparser
      (mconcat
        [ cmd createKeyP        "create-key"         "Create a key"
        , cmd listKeysP         "list-keys"          "List keys"
        , cmd deleteKeyP        "delete-key"         "Delete a key"
        , cmd createBucketP     "create-bucket"      "Create a bucket"
        , cmd listBucketsP      "list-buckets"       "List buckets"
        , cmd deleteBucketP     "delete-bucket"      "Delete a bucket"
        , cmd uploadFileP       "upload-file"        "Upload file"
        , cmd listFileNamesP    "list-file-names"    "List file names"
        , cmd listFileVersionsP "list-file-versions" "List file versions"
        , cmd downloadByIdP     "download-by-id"     "Download a file by ID"
        ])
   where
    cmd p name desc =
      command name (info (helper <*> p) (progDesc desc))
    createKeyP = CreateKey
      <$> argument (csv Env.str) (metavar "CAPABILITIES")
      <*> argument str (metavar "NAME")
      <*> argument auto (metavar "DURATION_S")
      <*> optional (option str (long "bucket" <> metavar "BUCKET"))
      <*> optional (option str (long "prefix" <> metavar "PREFIX"))
    listKeysP = ListKeys
      <$> optional (option auto (long "max-count" <> metavar "COUNT"))
      <*> optional (option str (long "start-key-id" <> metavar "KEY_ID"))
    deleteKeyP = DeleteKey
      <$> argument str (metavar "KEY_ID")
    createBucketP = CreateBucket
      <$> argument
            (eitherReader bucketType)
            (metavar "TYPE" <> help "'all-public', 'all-private', or 'snapshot'")
      <*> argument str (metavar "NAME")
    listBucketsP = ListBuckets
      <$> optional (option str (long "id" <> metavar "ID"))
      <*> optional (option str (long "name" <> metavar "NAME"))
      <*> optional (option (csv bucketType) (long "types" <> metavar "TYPES"))
    deleteBucketP = DeleteBucket
      <$> argument str (metavar "ID")
    uploadFileP = UploadFile
      <$> argument str (metavar "BUCKET")
      <*> argument str (metavar "FILENAME")
      <*> argument str (metavar "FILEPATH")
      <*> optional (option str (long "content-type" <> metavar "CONTENT-TYPE"))
    listFileNamesP = ListFileNames
      <$> argument str (metavar "BUCKET")
      <*> optional (option str (long "start-file-name" <> metavar "FILENAME"))
      <*> optional (option auto (long "max-count"))
      <*> optional (option str (long "prefix" <> metavar "FILENAME"))
      <*> optional (option char (long "delimiter" <> metavar "CHARACTER"))
    listFileVersionsP = ListFileVersions
      <$> argument str (metavar "BUCKET")
      <*> optional (option str (long "start-file-name" <> metavar "FILENAME"))
      <*> optional (option str (long "start-file-id" <> metavar "FILE"))
      <*> optional (option auto (long "max-count"))
      <*> optional (option str (long "prefix" <> metavar "FILENAME"))
      <*> optional (option char (long "delimiter" <> metavar "CHARACTER"))
    downloadByIdP = DownloadById
      <$> argument str (metavar "FILE")
      <*> argument str (metavar "FILEPATH")
      <*> optional (option auto (long "first-byte"))
      <*> optional (option auto (long "last-byte"))

char :: ReadM Char
char =
  eitherReader $ \case
    [x] ->
      pure x
    _ ->
      Left "must be a one-character string"

csv :: Env.Reader String a -> ReadM [a]
csv r =
  eitherReader (traverse r <=< Env.splitOn ',')

bucketType :: Env.Reader String B2.BucketType
bucketType = \case
  "all-private" ->
    pure B2.AllPrivate
  "all-public" ->
    pure B2.AllPublic
  "snapshot" ->
    pure B2.Snapshot
  _ ->
    Left "Unknown bucket type"
