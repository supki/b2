{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module App
  ( run
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import qualified Data.Aeson as Aeson
import           Data.Conduit (ConduitT, (.|), runConduit)
import qualified Data.Conduit.Binary as CB
import           Data.ByteString (ByteString)
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
  token <- dieW (B2.authorize_account B2.defaultBaseUrl cfgKeyID cfgApplicationKey man)
  case cmd of
    CreateKey capabilities name durationS bucket prefix ->
      dieP (B2.create_key token capabilities name durationS (fmap (\b -> (b, prefix)) bucket) man)
    ListKeys maxCount startKeyID ->
      dieP (B2.list_keys token maxCount startKeyID man)
    DeleteKey keyID ->
      dieP (B2.delete_key token keyID man)
    CreateBucket type_ name info ->
      dieP (B2.create_bucket token name type_ info Nothing Nothing man)
    ListBuckets bucket name type_ ->
      dieP (B2.list_buckets token bucket name type_ man)
    UpdateBucket bucket type_ info revision ->
      dieP (B2.update_bucket token bucket type_ info Nothing Nothing revision man)
    DeleteBucket bucket ->
      dieP (B2.delete_bucket token bucket man)
    UploadFile bucket filename filepath contentType info -> do
      uploadUrl <- dieW (B2.get_upload_url token bucket man)
      dieP (B2.upload_file uploadUrl filename contentType filepath info man)
    ListFileNames bucket startFileName maxCount prefix delimiter ->
      dieP (B2.list_file_names token bucket startFileName maxCount prefix delimiter man)
    ListFileVersions bucket startFileName startFileId maxCount prefix delimiter -> do
      let start = fmap (\n -> (n, startFileId)) startFileName
      dieP (B2.list_file_versions token bucket start maxCount prefix delimiter man)
    GetFileInfo file ->
      dieP (B2.get_file_info token file man)
    DownloadById file filepath firstByte lastByte -> do
      runResourceT $ do
        source <- dieW (B2.download_file_by_id token (firstByte, lastByte) file man)
        runConduit $
          source .| sinkFile filepath
    HideFile bucket filename ->
      dieP (B2.hide_file token bucket filename man)

sinkFile :: MonadResource m => FilePath -> ConduitT ByteString o m ()
sinkFile = \case
  "-" ->
    CB.sinkHandle IO.stdout
  path ->
    CB.sinkFile path

dieW :: (MonadIO m, Aeson.ToJSON e) => m (Either e a) -> m a
dieW x = do
  res <- x
  either dieJson pure res

dieP :: (Aeson.ToJSON e, Aeson.ToJSON a) => IO (Either e a) -> IO ()
dieP x = do
  res <- x
  either dieJson printJson res

dieJson :: (MonadIO m, Aeson.ToJSON e) => e -> m a
dieJson err = liftIO $ do
  hPrintJson IO.stderr err
  exitFailure

printJson :: Aeson.ToJSON a => a -> IO ()
printJson =
  hPrintJson IO.stdout

hPrintJson :: Aeson.ToJSON a => IO.Handle -> a -> IO ()
hPrintJson h =
  ByteString.Lazy.hPutStrLn h . Aeson.encode
