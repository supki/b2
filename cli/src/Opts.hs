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
        [ command "create-key" (info (helper <*> createKeyP) (progDesc "Create a key"))
        , command "list-keys" (info (helper <*> listKeysP) (progDesc "List keys"))
        , command "delete-key" (info (helper <*> deleteKeyP) (progDesc "Delete a key"))
        , command "create-bucket" (info (helper <*> createBucketP) (progDesc "Create a bucket"))
        , command "list-buckets" (info (helper <*> listBucketsP) (progDesc "List buckets"))
        , command "delete-bucket" (info (helper <*> deleteBucketP) (progDesc "Delete a bucket"))
        , command "upload-file" (info (helper <*> uploadFileP) (progDesc "Upload file"))
        ])
   where
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
