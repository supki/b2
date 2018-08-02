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
    deriving (Show, Eq)

get :: IO Cmd
get =
  execParser (info (parser <**> helper) (fullDesc <> header Cfg.usageHeader))
 where
  parser =
    subparser (command "create-key" (info createKeyP (progDesc "Create a new key")))
   where
    createKeyP = CreateKey
      <$> argument csv (metavar "CAPABILITIES")
      <*> argument str (metavar "NAME")
      <*> argument auto (metavar "DURATION_S")
      <*> optional (option str (long "bucket" <> metavar "BUCKET"))
      <*> optional (option str (long "prefix" <> metavar "PREFIX"))

csv :: IsString str => ReadM [str]
csv =
  eitherReader (fmap (map fromString) . Env.splitOn ',')
