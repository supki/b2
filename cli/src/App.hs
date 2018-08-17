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
    UploadFile bucket filename filepath contentType -> do
      uploadUrl <- dieW (B2.b2_get_upload_url token bucket man)
      contents <- fileContents filepath
      dieP (B2.b2_upload_file uploadUrl filename contentType contents [] man)
    ListFileNames bucket startFileName maxCount prefix delimiter ->
      dieP (B2.b2_list_file_names token bucket startFileName maxCount prefix delimiter man)
    ListFileVersions bucket startFileName startFileId maxCount prefix delimiter -> do
      let start = fmap (\n -> (n, startFileId)) startFileName
      dieP (B2.b2_list_file_versions token bucket start maxCount prefix delimiter man)
    DownloadById file filepath firstByte lastByte -> do
      runResourceT $ do
        source <- dieW (B2.b2_download_file_by_id token (firstByte, lastByte) file man)
        runConduit $
          source .| sinkFile filepath

fileContents :: FilePath -> IO Lazy.ByteString
fileContents = \case
  "-" ->
    ByteString.Lazy.hGetContents IO.stdin
  path ->
    ByteString.Lazy.readFile path

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
