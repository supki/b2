{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module App
  ( run
  ) where

import           Control.Exception (bracket_)
import           Control.Concurrent.Async (async, wait)
import qualified Control.Concurrent.QSem as QSem
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import qualified Data.Aeson as Aeson
import           Data.Bool (bool)
import           Data.Conduit (ConduitT, (.|), runConduit)
import qualified Data.Conduit.Binary as CB
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy
import           Data.Int (Int64)
import           Data.Traversable (for, traverse)
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
    UploadFile bucket filename path contentType info -> do
      uploadUrl <- dieW (B2.get_upload_url token bucket man)
      size <- fileSize path
      dieP (B2.upload_file uploadUrl filename size (CB.sourceFile path) contentType info man)
    UploadLargeFile bucket filename path contentType info -> do
      largeFile <- dieW (B2.start_large_file token bucket filename contentType info man)
      size <- fileSize path
      let partSize = B2.recommendedPartSize token
          partsCount = size `div` partSize + bool 0 1 (size `mod` partSize > 0)
          threads = 3
      sem <- QSem.newQSem threads
      asyncs <- for [1 .. partsCount] $ \partNumber ->
        bracket_ (QSem.waitQSem sem) (QSem.signalQSem sem) $
          async $ do
            url <- dieW (B2.get_upload_part_url token largeFile man)
            let offset = fromIntegral ((partNumber - 1) * partSize)
                maxCount = fromIntegral partSize
                src = CB.sourceFileRange path (pure offset) (pure maxCount)
                partActualSize = bool partSize (size `mod` partSize) (partNumber == partsCount)
            dieW (B2.upload_part url partNumber partActualSize src man)
      parts <- traverse wait asyncs
      dieP (B2.finish_large_file token largeFile parts man)
    ListFileNames bucket startFileName maxCount prefix delimiter ->
      dieP (B2.list_file_names token bucket startFileName maxCount prefix delimiter man)
    ListFileVersions bucket startFileName startFileId maxCount prefix delimiter -> do
      let start = fmap (\n -> (n, startFileId)) startFileName
      dieP (B2.list_file_versions token bucket start maxCount prefix delimiter man)
    ListUnfinishedLargeFiles bucket prefix startFileId maxCount -> do
      dieP (B2.list_unfinished_large_files token bucket prefix startFileId maxCount man)
    GetFileInfo file ->
      dieP (B2.get_file_info token file man)
    GetDownloadAuth bucket prefix durationS ->
      dieP (B2.get_download_authorization token bucket prefix durationS Nothing man)
    DownloadById file path firstByte lastByte ->
      download (dieW (B2.download_file_by_id token (firstByte, lastByte) file man)) path
    DownloadByName bucket filename path firstByte lastByte ->
      download (dieW (B2.download_file_by_name token (firstByte, lastByte) bucket filename man)) path
    HideFile bucket filename ->
      dieP (B2.hide_file token bucket filename man)
    DeleteFileVersion filename file ->
      dieP (B2.delete_file_version token filename file man)

download :: ResourceT IO (ConduitT () ByteString (ResourceT IO) ()) -> FilePath -> IO ()
download streamBody path =
  runResourceT $ do
    source <- streamBody
    runConduit $
      source .| sinkFile path

sinkFile :: MonadResource m => FilePath -> ConduitT ByteString o m ()
sinkFile = \case
  "-" ->
    CB.sinkHandle IO.stdout
  path ->
    CB.sinkFile path

fileSize :: FilePath -> IO Int64
fileSize path =
  fmap fromIntegral (IO.withBinaryFile path IO.ReadMode IO.hFileSize)

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
