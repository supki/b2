{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module App
  ( run
  ) where

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Aeson as Aeson
import           Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy
import qualified Network.HTTP.Conduit as Http
import           System.Exit (exitFailure)
import qualified System.IO as IO

import           Cfg (Cfg(..))
import           Opts (Cmd(..))

import qualified B2


run :: Cfg -> Cmd -> IO ()
run Cfg {..} cmd = do
  man <- Http.newManager Http.tlsManagerSettings
  token <- dieW (B2.b2_authorize_account B2.defaultBaseUrl cfgKeyID cfgApplicationKey man)
  case cmd of
    CreateKey capabilities name durationS bucket prefix ->
      dieP (B2.b2_create_key token capabilities name durationS (fmap (\b -> (b, prefix)) bucket) man)
    ListKeys maxCount startKeyID ->
      dieP (B2.b2_list_keys token maxCount startKeyID man)
    DeleteKey keyID ->
      dieP (B2.b2_delete_key token keyID man)
    CreateBucket type_ name ->
      dieP (B2.b2_create_bucket token name type_ Nothing Nothing Nothing man)
    ListBuckets id_ name type_ ->
      dieP (B2.b2_list_buckets token id_ name type_ man)
    DeleteBucket id_ ->
      dieP (B2.b2_delete_bucket token id_ man)
    UploadFile bucket filename filepath -> do
      uploadUrl <- dieW (B2.b2_get_upload_url token bucket man)
      contents <- fileContents filepath
      dieP (B2.b2_upload_file uploadUrl filename Nothing contents [] man)
    ListFileNames bucket startFileName maxCount prefix delimiter ->
      dieP (B2.b2_list_file_names token bucket startFileName maxCount prefix delimiter man)
    DownloadById file firstByte lastByte -> do
      runResourceT $ do
        source <- B2.b2_download_file_by_id token (firstByte, lastByte) file man
        runConduit $
          source .| CB.sinkHandle IO.stdout

fileContents :: FilePath -> IO Lazy.ByteString
fileContents = \case
  "-" ->
    ByteString.Lazy.hGetContents IO.stdin
  path ->
    ByteString.Lazy.readFile path

dieW :: Aeson.ToJSON e => IO (Either e a) -> IO a
dieW x = do
  res <- x
  either dieJson pure res

dieP :: (Aeson.ToJSON e, Aeson.ToJSON a) => IO (Either e a) -> IO ()
dieP x = do
  res <- x
  either dieJson printJson res

dieJson :: Aeson.ToJSON e => e -> IO a
dieJson err = do
  hPrintJson IO.stderr err
  exitFailure

printJson :: Aeson.ToJSON a => a -> IO ()
printJson =
  hPrintJson IO.stdout

hPrintJson :: Aeson.ToJSON a => IO.Handle -> a -> IO ()
hPrintJson h =
  ByteString.Lazy.hPutStrLn h . Aeson.encode
