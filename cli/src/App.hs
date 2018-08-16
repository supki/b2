{-# LANGUAGE RecordWildCards #-}
module App
  ( run
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy
import qualified Network.HTTP.Conduit as Http
import           System.Exit (exitFailure)
import           System.IO (Handle, stderr, stdout)

import           Cfg (Cfg(..))
import           Opts (Cmd(..))

import qualified B2


run :: Cfg -> Cmd -> IO ()
run Cfg {..} cmd = do
  man <- Http.newManager Http.tlsManagerSettings
  tokenE <- B2.b2_authorize_account B2.defaultBaseUrl cfgKeyID cfgApplicationKey man
  token <- dieE tokenE
  case cmd of
    CreateKey capabilities name durationS bucket prefix -> do
      res <- B2.b2_create_key token capabilities name durationS (fmap (\b -> (b, prefix)) bucket) man
      either dieJson printJson res
    ListKeys maxCount startKeyID -> do
      res <- B2.b2_list_keys token maxCount startKeyID man
      either dieJson printJson res
    DeleteKey keyID -> do
      res <- B2.b2_delete_key token keyID man
      either dieJson printJson res
    CreateBucket type_ name -> do
      res <- B2.b2_create_bucket token name type_ Nothing Nothing Nothing man
      either dieJson printJson res
    ListBuckets id_ name type_ -> do
      res <- B2.b2_list_buckets token id_ name type_ man
      either dieJson printJson res

dieE :: Aeson.ToJSON e => Either e a -> IO a
dieE =
  either dieJson pure

dieJson :: Aeson.ToJSON e => e -> IO a
dieJson err = do
  hPrintJson stderr err
  exitFailure

printJson :: Aeson.ToJSON a => a -> IO ()
printJson =
  hPrintJson stdout

hPrintJson :: Aeson.ToJSON a => Handle -> a -> IO ()
hPrintJson h =
  ByteString.Lazy.hPutStrLn h . Aeson.encode
