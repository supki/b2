{-# LANGUAGE StrictData #-}
module Opts
  ( Cmd(..)
  , get
  ) where

import           Options.Applicative
import           Data.Int (Int64)
import           Data.String (IsString(..))
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
        [ command "create-key" (info (helper <*> createKeyP) (progDesc "Create a new key"))
        , command "list-keys" (info (helper <*> listKeysP) (progDesc "List keys"))
        , command "delete-key" (info (helper <*> deleteKeyP) (progDesc "Delete a key"))
        ])
   where
    createKeyP = CreateKey
      <$> argument csv (metavar "CAPABILITIES")
      <*> argument str (metavar "NAME")
      <*> argument auto (metavar "DURATION_S")
      <*> optional (option str (long "bucket" <> metavar "BUCKET"))
      <*> optional (option str (long "prefix" <> metavar "PREFIX"))
    listKeysP = ListKeys
      <$> optional (option auto (long "max-count" <> metavar "COUNT"))
      <*> optional (option str (long "start-key-id" <> metavar "KEY_ID"))
    deleteKeyP = DeleteKey
      <$> argument str (metavar "KEY_ID")

csv :: IsString str => ReadM [str]
csv =
  eitherReader (fmap (map fromString) . Env.splitOn ',')
