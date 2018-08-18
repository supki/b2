{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module Opts
  ( Cmd(..)
  , get
  ) where

import           Control.Monad ((<=<))
import           Options.Applicative
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
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
      (Maybe (HashMap Text Text))
  | ListBuckets
      (Maybe (B2.ID B2.Bucket))
      (Maybe Text)
      (Maybe [B2.BucketType])
  | UpdateBucket
      (B2.ID B2.Bucket)
      (Maybe B2.BucketType)
      (Maybe (HashMap Text Text))
      (Maybe Int64)
  | DeleteBucket
      (B2.ID B2.Bucket)
  | UploadFile
      (B2.ID B2.Bucket)
      Text
      FilePath
      (Maybe Text)
      (Maybe (HashMap Text Text))
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
  | GetFileInfo
      (B2.ID B2.File)
  | DownloadById
      (B2.ID B2.File)
      FilePath
      (Maybe Int64)
      (Maybe Int64)
  | HideFile
      (B2.ID B2.Bucket)
      Text
  | DeleteFileVersion
      B2.FileName
      (B2.ID B2.File)
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
        [ cmd createKeyP         "create-key"          "Create a key"
        , cmd listKeysP          "list-keys"           "List keys"
        , cmd deleteKeyP         "delete-key"          "Delete a key"

        , cmd createBucketP      "create-bucket"       "Create a bucket"
        , cmd listBucketsP       "list-buckets"        "List buckets"
        , cmd updateBucketP      "update-bucket"       "Update a bucket"
        , cmd deleteBucketP      "delete-bucket"       "Delete a bucket"

        , cmd uploadFileP        "upload-file"         "Upload file"
        , cmd listFileNamesP     "list-file-names"     "List file names"
        , cmd listFileVersionsP  "list-file-versions"  "List file versions"
        , cmd getFileInfoP       "get-file-info"       "Get file info"
        , cmd downloadByIdP      "download-by-id"      "Download a file by ID"
        , cmd hideFileP          "hide-file"           "Hide a file"
        , cmd deleteFileVersionP "delete-file-version" "Delete a file version"
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
      <*> optional (option hashmap (long "info"))
    listBucketsP = ListBuckets
      <$> optional (option str (long "id" <> metavar "ID"))
      <*> optional (option str (long "name" <> metavar "NAME"))
      <*> optional (option (csv bucketType) (long "types" <> metavar "TYPES"))
    updateBucketP = UpdateBucket
      <$> argument str (metavar "BUCKET")
      <*> optional
            (option
              (eitherReader bucketType)
              (long "type" <> metavar "TYPE" <> help "'all-public', 'all-private', or 'snapshot'"))
      <*> optional (option hashmap (long "info"))
      <*> optional (option auto (long "revision"))
    deleteBucketP = DeleteBucket
      <$> argument str (metavar "ID")
    uploadFileP = UploadFile
      <$> argument str (metavar "BUCKET")
      <*> argument str (metavar "FILENAME")
      <*> argument str (metavar "FILEPATH")
      <*> optional (option str (long "content-type" <> metavar "CONTENT-TYPE"))
      <*> optional (option hashmap (long "info"))
    listFileNamesP = ListFileNames
      <$> argument str (metavar "BUCKET")
      <*> optional (option str (long "start-name" <> metavar "FILENAME"))
      <*> optional (option auto (long "max-count"))
      <*> optional (option str (long "prefix" <> metavar "FILENAME"))
      <*> optional (option char (long "delimiter" <> metavar "CHARACTER"))
    listFileVersionsP = ListFileVersions
      <$> argument str (metavar "BUCKET")
      <*> optional (option str (long "start-name" <> metavar "FILENAME"))
      <*> optional (option str (long "start-id" <> metavar "FILE"))
      <*> optional (option auto (long "max-count"))
      <*> optional (option str (long "prefix" <> metavar "FILENAME"))
      <*> optional (option char (long "delimiter" <> metavar "CHARACTER"))
    getFileInfoP = GetFileInfo
      <$> argument str (metavar "FILE")
    downloadByIdP = DownloadById
      <$> argument str (metavar "FILE")
      <*> argument str (metavar "FILEPATH")
      <*> optional (option auto (long "first-byte"))
      <*> optional (option auto (long "last-byte"))
    hideFileP = HideFile
      <$> argument str (metavar "BUCKET")
      <*> argument str (metavar "FILENAME")
    deleteFileVersionP = DeleteFileVersion
      <$> argument str (metavar "FILENAME")
      <*> argument str (metavar "FILE")

hashmap :: ReadM (HashMap Text Text)
hashmap =
  eitherReader $ replaceLeft msg . \s -> do
    xs <- Env.splitOn ',' s
    ys <- traverse (pair <=< traverse Env.str <=< Env.splitOn ':') xs
    pure (HashMap.fromList ys)
 where
  msg = "must be a comma-separated list of colon-separated key-value pairs"

replaceLeft :: b -> Either a x -> Either b x
replaceLeft b =
  either (\_ -> Left b) pure

csv :: Env.Reader String a -> ReadM [a]
csv =
  ssv ','

ssv :: Char -> Env.Reader String a -> ReadM [a]
ssv c r =
  eitherReader (traverse r <=< Env.splitOn c)

pair :: [a] -> Either String (a, a)
pair = \case
  [x, y] ->
    pure (x, y)
  _ ->
    Left "must be a pair"

char :: ReadM Char
char =
  eitherReader $ \case
    [x] ->
      pure x
    _ ->
      Left "must be a one-character string"

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
