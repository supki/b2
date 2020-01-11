{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module B2
  ( module B2.AuthorizationToken
  , module B2.Bucket
  , module B2.File
  , module B2.ID
  , module B2.Key
  , module B2.LargeFile
  , module B2.Upload
  , module B2.Url
  , module B2
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (Exception, throwIO, SomeException, catch)
import           Control.Monad (join)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource, ResourceT)
import qualified Crypto.Hash as Hash
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Pair)
import           Data.Bifunctor (bimap)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.CaseInsensitive as CI
import           Data.Conduit (ConduitT, (.|), await, runConduit, yield, Void, handleC)
import qualified Data.Conduit.List as CL
import           Data.Conduit.Combinators (sourceLazy)
import           Data.Int (Int64)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.String (IsString(fromString))
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Prelude hiding (id)
import qualified Network.HTTP.Conduit as Http
import qualified Network.HTTP.Types as Http
import           Text.Printf (printf)

import           B2.AuthorizationToken
import           B2.Bucket
import           B2.ID
import           B2.File
import           B2.LargeFile
import           B2.Key
import           B2.Upload
import           B2.Url


data Error = Error
  { code    :: Text
  , message :: Text
  , status  :: Int64
  } deriving (Show, Eq)

instance Aeson.FromJSON Error where
  parseJSON =
    Aeson.withObject "Error" $ \o -> do
      code <- o .: "code"
      message <- o .: "message"
      status <- o .: "status"
      pure Error {..}

instance Aeson.ToJSON Error where
  toJSON Error {..} =
    Aeson.object
      [ "code" .= code
      , "message" .= message
      , "status" .= status
      ]

instance Exception Error 

data Ex
  = JsonEx Lazy.ByteString String
    deriving (Show, Eq)

instance Exception Ex

authorize_account
  :: HasBaseUrl url
  => url
  -> ID Key
  -> ApplicationKey
  -> Http.Manager
  -> IO (Either Error AuthorizeAccount)
authorize_account url keyID applicationKey man = do
  req <- basicRequest url keyID applicationKey "/b2api/v1/b2_authorize_account"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS "{}"
    } man
  parseResponseJson res

cancel_large_file
  :: ( HasFileID fileID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> fileID
  -> Http.Manager
  -> IO (Either Error FileIDs)
cancel_large_file env file man = do
  req <- tokenRequest env "/b2api/v1/b2_cancel_large_file"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "fileId" .= getFileID file
        ]
    } man
  parseResponseJson res

create_bucket
  :: ( HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     )
  => env
  -> Text
  -> BucketType
  -> Maybe (HashMap Text Text)
  -> Maybe [CorsRule]
  -> Maybe [LifecycleRule]
  -> Http.Manager
  -> IO (Either Error Bucket)
create_bucket env name type_ info cors lifecycle man = do
  req <- tokenRequest env "/b2api/v1/b2_create_bucket"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "accountId" .= getAccountID env
        , "bucketName" .= name
        , "bucketType" .= type_
        , "bucketInfo" .= info
        , "corsRules" .= cors
        , "lifecycleRules" .= lifecycle
        ]
    } man
  parseResponseJson res

create_key
  :: ( HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     )
  => env
  -> [Capability]
  -> Text
  -> Int64
  -> Maybe (ID Bucket, Maybe Text)
  -> Http.Manager
  -> IO (Either Error (Key ApplicationKey))
create_key env capabilities name durationS restrictions man = do
  req <- tokenRequest env "/b2api/v1/b2_create_key"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "accountId" .= getAccountID env
        , "capabilities" .= capabilities
        , "keyName" .= name
        , "validDurationInSeconds" .= durationS
        , "bucketId" .= fmap fst restrictions
        , "namePrefix" .= join (fmap snd restrictions)
        ]
    } man
  parseResponseJson res

delete_bucket
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Http.Manager
  -> IO (Either Error Bucket)
delete_bucket env id man = do
  req <- tokenRequest env "/b2api/v1/b2_delete_bucket"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "accountId" .= getAccountID env
        , "bucketId" .= getBucketID id
        ]
    } man
  parseResponseJson res

delete_file_version
  :: ( HasFileName fileName
     , HasFileID fileID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> fileName
  -> fileID
  -> Http.Manager
  -> IO (Either Error FileIDs)
delete_file_version env name id man = do
  req <- tokenRequest env "/b2api/v1/b2_delete_file_version"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "fileName" .= getFileName name
        , "fileId" .= getFileID id
        ]
    } man
  parseResponseJson res

delete_key
  :: ( HasKeyID keyID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> keyID
  -> Http.Manager
  -> IO (Either Error (Key NoSecret))
delete_key env id man = do
  req <- tokenRequest env "/b2api/v1/b2_delete_key"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "applicationKeyId" .= getKeyID id
        ]
    } man
  parseResponseJson res

download_file_by_id
  :: ( HasFileID fileID
     , HasDownloadUrl env
     , HasAuthorizationToken env
     , MonadResource m
     )
  => env
  -> (Maybe Int64, Maybe Int64)
  -> fileID
  -> Http.Manager
  -> m (Either Error (ConduitT () ByteString m ()))
download_file_by_id env range fileID man = do
  req <- downloadByIDRequest env range fileID
  res <- Http.http req man
  parseResponseConduit res

download_file_by_name
  :: ( HasDownloadUrl env
     , HasAuthorizationToken env
     , HasFileName fileName
     , MonadResource m
     )
  => env
  -> (Maybe Int64, Maybe Int64)
  -> Text
  -> fileName
  -> Http.Manager
  -> m (Either Error (ConduitT () ByteString m ()))
download_file_by_name env range bucketName fileName man = do
  req <- downloadByNameRequest env range bucketName fileName
  res <- Http.http req man
  parseResponseConduit res

finish_large_file
  :: ( HasFileID fileID
     , HasBaseUrl env
     , HasAuthorizationToken env
     , HasPartSha1 part
     )
  => env
  -> fileID
  -> [part]
  -> Http.Manager
  -> IO (Either Error File)
finish_large_file env file partSha1Array man = do
  req <- tokenRequest env "/b2api/v1/b2_finish_large_file"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "fileId" .= getFileID file
        , "partSha1Array" .= map getPartSha1 partSha1Array
        ]
    } man
  parseResponseJson res

get_download_authorization
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Text
  -> Int64
  -> Maybe Text
  -> Http.Manager
  -> IO (Either Error DownloadAuthorization)
get_download_authorization env bucket fileNamePrefix durationS disposition man = do
  req <- tokenRequest env "/b2api/v1/b2_get_download_authorization"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "bucketId" .= getBucketID bucket
        , "fileNamePrefix" .= fileNamePrefix
        , "validDurationInSeconds" .= durationS
        , "b2ContentDisposition" .= disposition
        ]
    } man
  parseResponseJson res

get_file_info
  :: ( HasFileID fileID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> fileID
  -> Http.Manager
  -> IO (Either Error File)
get_file_info env id man = do
  req <- tokenRequest env "/b2api/v1/b2_get_file_info"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "fileId" .= getFileID id
        ]
    } man
  parseResponseJson res

get_upload_url
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Http.Manager
  -> IO (Either Error UploadInfo)
get_upload_url env id man = do
  req <- tokenRequest env "/b2api/v1/b2_get_upload_url"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "bucketId" .= getBucketID id
        ]
    } man
  parseResponseJson res

get_upload_part_url
  :: ( HasFileID fileID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> fileID
  -> Http.Manager
  -> IO (Either Error UploadPartInfo)
get_upload_part_url env id man = do
  req <- tokenRequest env "/b2api/v1/b2_get_upload_part_url"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "fileId" .= getFileID id
        ]
    } man
  parseResponseJson res

hide_file
  :: ( HasBucketID bucketID
     , HasFileName fileName
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> fileName
  -> Http.Manager
  -> IO (Either Error File)
hide_file env bucket fileName man = do
  req <- tokenRequest env "/b2api/v1/b2_hide_file"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "bucketId" .= getBucketID bucket
        , "fileName" .= getFileName fileName
        ]
    } man
  parseResponseJson res

list_buckets
  :: ( HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     )
  => env
  -> Maybe (ID Bucket)
  -> Maybe Text
  -> Maybe [BucketType]
  -> Http.Manager
  -> IO (Either Error [Bucket])
list_buckets env bucket name types man = do
  req <- tokenRequest env "/b2api/v1/b2_list_buckets"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "accountId" .= getAccountID env
        , "bucketId" .= bucket
        , "bucketName" .= name
        , "bucketTypes" .= types
        ]
    } man
  fmap (fmap unBuckets) (parseResponseJson res)

list_file_names
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Maybe Text
  -> Maybe Int64
  -> Maybe Text
  -> Maybe Char
  -> Http.Manager
  -> IO (Either Error Files)
list_file_names env id startName maxCount prefix delimiter man = do
  req <- tokenRequest env "/b2api/v1/b2_list_file_names"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "bucketId" .= getBucketID id
        , "startFileName" .= startName
        , "maxFileCount" .= maxCount
        , "prefix" .= prefix
        , "delimiter" .= delimiter
        ]
    } man
  parseResponseJson res

list_file_versions
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Maybe (Text, Maybe (ID File))
  -> Maybe Int64
  -> Maybe Text
  -> Maybe Char
  -> Http.Manager
  -> IO (Either Error Files)
list_file_versions env id startName maxCount prefix delimiter man = do
  req <- tokenRequest env "/b2api/v1/b2_list_file_versions"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "bucketId" .= getBucketID id
        , "startFileName" .= fmap fst startName
        , "startFileId" .= join (fmap snd startName)
        , "maxFileCount" .= maxCount
        , "prefix" .= prefix
        , "delimiter" .= delimiter
        ]
    } man
  parseResponseJson res

list_keys
  :: ( HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     )
  => env
  -> Maybe Int64
  -> Maybe (ID Key)
  -> Http.Manager
  -> IO (Either Error Keys)
list_keys env maxKeyCount startApplicationKeyID man = do
  req <- tokenRequest env "/b2api/v1/b2_list_keys"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "accountId" .= getAccountID env
        , "maxKeyCount" .= maxKeyCount
        , "startApplicationKeyId" .= startApplicationKeyID
        ]
    } man
  parseResponseJson res

list_parts
  :: ( HasFileID fileID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> fileID
  -> Maybe Int64
  -> Maybe Int64
  -> Http.Manager
  -> IO (Either Error LargeFileParts)
list_parts env file startPartNumber maxCount man = do
  req <- tokenRequest env "/b2api/v1/b2_list_parts"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "fileId" .= getFileID file
        , "startPartNumber" .= startPartNumber
        , "maxPartCount" .= maxCount
        ]
    } man
  parseResponseJson res

list_unfinished_large_files
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Maybe Text
  -> Maybe (ID File)
  -> Maybe Int64
  -> Http.Manager
  -> IO (Either Error LargeFiles)
list_unfinished_large_files env bucket namePrefix startFileID maxCount man = do
  req <- tokenRequest env "/b2api/v1/b2_list_unfinished_large_files"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "bucketId" .= getBucketID bucket
        , "namePrefix" .= namePrefix
        , "startFileId" .= startFileID
        , "maxFileCount" .= maxCount
        ]
    } man
  parseResponseJson res

start_large_file
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Text
  -> Maybe Text
  -> Maybe (HashMap Text Text)
  -> Http.Manager
  -> IO (Either Error LargeFile)
start_large_file env bucket fileName contentType info man = do
  req <- tokenRequest env "/b2api/v1/b2_start_large_file"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "bucketId" .= getBucketID bucket
        , "fileName" .= fileName
        , "contentType" .= fromMaybe autoContentType contentType
        , "fileInfo" .= info
        ]
    } man
  parseResponseJson res

update_bucket
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Maybe BucketType
  -> Maybe (HashMap Text Text)
  -> Maybe [CorsRule]
  -> Maybe [LifecycleRule]
  -> Maybe Int64
  -> Http.Manager
  -> IO (Either Error Bucket)
update_bucket env bucket type_ info cors lifecycle revision man = do
  req <- tokenRequest env "/b2api/v1/b2_update_bucket"
  res <- Http.httpLbs req
    { Http.requestBody=requestBodyJson
        [ "accountId" .= getAccountID env
        , "bucketId" .= getBucketID bucket
        , "bucketType" .= type_
        , "bucketInfo" .= info
        , "corsRules" .= cors
        , "lifecycleRules" .= lifecycle
        , "ifRevisionIs" .= revision
        ]
    } man
  parseResponseJson res

upload_file
  :: ( HasUploadUrl env
     , HasAuthorizationToken env
     )
  => env
  -> Text
  -> Int64
  -> ConduitT () ByteString (ResourceT IO) ()
  -> Maybe Text
  -> Maybe (HashMap Text Text)
  -> Http.Manager
  -> IO (Either Error File)
upload_file env name size contents contentType info man = do
  req <- uploadRequest env name size contents contentType info
  res <- Http.httpLbs req man
  parseResponseJson res

upload_part
  :: ( HasUploadPartUrl env
     , HasAuthorizationToken env
     )
  => env
  -> Int64
  -> Int64
  -> ConduitT () ByteString (ResourceT IO) ()
  -> Http.Manager
  -> IO (Either Error LargeFilePart)
upload_part env idx size contents man = do
  req <- uploadPartRequest env idx size contents
  res <- Http.httpLbs req man
  parseResponseJson res

basicRequest
  :: HasBaseUrl env
  => env
  -> ID Key
  -> ApplicationKey
  -> String
  -> IO Http.Request
basicRequest env ID {unID} ApplicationKey {unApplicationKey} method = do
  req <- request env method
  pure (applyAuth req)
 where
  applyAuth =
    Http.applyBasicAuth (Text.encodeUtf8 unID) (Text.encodeUtf8 unApplicationKey)

tokenRequest
  :: ( HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> String
  -> IO Http.Request
tokenRequest env method = do
  req <- request env method
  pure req
    { Http.requestHeaders=authorization env : Http.requestHeaders req
    }

request :: HasBaseUrl env => env -> String -> IO Http.Request
request url method = do
  req <- Http.parseRequest (unBaseUrl (getBaseUrl url) <> method)
  pure req
    { Http.method="POST"
    }

uploadRequest
  :: (HasUploadUrl env, HasAuthorizationToken env)
  => env
  -> Text
  -> Int64
  -> ConduitT () ByteString (ResourceT IO) ()
  -> Maybe Text
  -> Maybe (HashMap Text Text)
  -> IO Http.Request
uploadRequest env name size contents contentType info = do
  req <- Http.parseRequest (unUploadUrl (getUploadUrl env))
  pure req
    { Http.method="POST"
    , Http.requestHeaders=
      ( authorization env
      : ("X-Bz-File-Name", urlEncode (Text.encodeUtf8 name))
      : ("Content-Type", maybe autoContentType Text.encodeUtf8 contentType)
      : ("X-Bz-Content-Sha1", "hex_digits_at_end")
      : map
          (bimap
            (("X-Bz-Info-" <>) . CI.mk . Text.encodeUtf8)
            (urlEncode . Text.encodeUtf8))
          (foldMap HashMap.toList info)
      )
    , Http.requestBody=
        Http.requestBodySource (size + 40) (contents .| hexDigitsAtEnd)
    }
 where
  urlEncode =
    Http.urlEncode True

uploadPartRequest
  :: (HasUploadPartUrl env, HasAuthorizationToken env)
  => env
  -> Int64
  -> Int64
  -> ConduitT () ByteString (ResourceT IO) ()
  -> IO Http.Request
uploadPartRequest env idx size contents = do
  req <- Http.parseRequest (unUploadPartUrl (getUploadPartUrl env))
  pure req
    { Http.method="POST"
    , Http.requestHeaders=
      ( authorization env
      : ("X-Bz-Part-Number", text idx)
      : ("X-Bz-Content-Sha1", "hex_digits_at_end")
      : []
      )
    , Http.requestBody=
        Http.requestBodySource (size + 40) (contents .| hexDigitsAtEnd)
    }
 where
  text :: Show a => a -> ByteString
  text =
    fromString . show

hexDigitsAtEnd :: Monad m => ConduitT ByteString ByteString m ()
hexDigitsAtEnd =
  go (Hash.hashInit @Hash.SHA1)
 where
  go ctx = do
    nextChunk <- await
    case nextChunk of
      Nothing ->
        yield (fromString (show (Hash.hashFinalize ctx)))
      Just chunk -> do
        yield chunk
        let ctx' = Hash.hashUpdate ctx chunk
        ctx' `seq` go ctx'

downloadByNameRequest
  :: ( HasDownloadUrl env
     , HasAuthorizationToken env
     , HasFileName fileName
     , MonadIO m
     )
  => env
  -> (Maybe Int64, Maybe Int64)
  -> Text
  -> fileName
  -> m Http.Request
downloadByNameRequest env range bucket file =
  downloadRequest env range (printf "/file/%s/%s" bucket (getFileName file))

downloadByIDRequest
  :: ( HasFileID fileID
     , HasDownloadUrl env
     , HasAuthorizationToken env
     , MonadIO m
     )
  => env
  -> (Maybe Int64, Maybe Int64)
  -> fileID
  -> m Http.Request
downloadByIDRequest env range file =
  downloadRequest env range method
 where
  method = printf "/b2api/v1/b2_download_file_by_id?fileId=%s" (getFileID file)

downloadRequest
  :: ( HasDownloadUrl env
     , HasAuthorizationToken env
     , MonadIO m
     )
  => env
  -> (Maybe Int64, Maybe Int64)
  -> String
  -> m Http.Request
downloadRequest env (from, to) method = liftIO $ do
  req <- Http.parseRequest (unDownloadUrl (getDownloadUrl env) <> method)
  pure req
    { Http.requestHeaders=
        ("Range", genRange) : authorization env : Http.requestHeaders req
    }
 where
  genRange =
    fromString (printf "bytes=%d-%s" (fromMaybe 0 from) (maybe "" show to))

parseResponseJson
  :: (Aeson.FromJSON err, Aeson.FromJSON a)
  => Http.Response Lazy.ByteString
  -> IO (Either err a)
parseResponseJson =
  parseResponse (fmap Left . parseJsonEx) (fmap Right . parseJsonEx)

parseResponseConduit
  :: (MonadIO m, Aeson.FromJSON err, body ~ (ConduitT () ByteString m ()))
  => Http.Response body
  -> m (Either err body)
parseResponseConduit =
  parseResponse onErr (pure . Right)
 where
  onErr body = do
    bytes <- fmap ByteString.Lazy.fromChunks (runConduit (body .| CL.consume))
    liftIO (fmap Left (parseJsonEx bytes))

parseResponse :: (body -> r) -> (body -> r) -> Http.Response body -> r
parseResponse onErr onOk res = do
  if 200 <= statusCode && statusCode < 300
    then onOk body
    else onErr body
 where
  body =
    Http.responseBody res
  Http.Status {statusCode} =
    Http.responseStatus res

parseJsonEx :: Aeson.FromJSON a => Lazy.ByteString -> IO a
parseJsonEx bytes =
  either (throwIO . JsonEx bytes) pure (Aeson.eitherDecode bytes)

authorization :: HasAuthorizationToken env => env -> Http.Header
authorization env =
  ("Authorization", Text.encodeUtf8 (unAuthorizationToken (getAuthorizationToken env)))

autoContentType :: IsString str => str
autoContentType =
  "b2/x-auto"

requestBodyJson :: [Aeson.Pair] -> Http.RequestBody
requestBodyJson pairs =
  Http.RequestBodyLBS (Aeson.encode (Aeson.object pairs))

streamUpload ::
  (HasBucketID bucketID) =>
  -- | Optional chunk size
  Maybe Int64 ->
  AuthorizeAccount ->
  bucketID ->
  Text ->
  Http.Manager ->
  ConduitT ByteString Void (ResourceT IO) File
streamUpload Nothing env bucketID filename manager =
  streamUpload (Just (recommendedPartSize env)) env bucketID filename manager
streamUpload (Just chunkSize) env bucketID filename manager = do
  go Nothing mempty 0 1 mempty
  where
    go :: Maybe LargeFile -> BSB.Builder -> Int64 -> Int64 -> [LargeFilePart] -> ConduitT ByteString Void (ResourceT IO) File
    go maybeFileID buffer bufferSize partNumber parts = handleC (handler maybeFileID) $
      await >>= \case
        Just bytes ->
          let newBufferSize = fromIntegral (BS.length bytes)
              totalBufferSize = bufferSize + newBufferSize
           in if totalBufferSize <= chunkSize
                then go maybeFileID (buffer <> BSB.byteString bytes) totalBufferSize partNumber parts
                else do
                  (part, fileID) <- uploadPart maybeFileID buffer bufferSize partNumber
                  go (Just fileID) (mempty <> BSB.byteString bytes) newBufferSize (partNumber + 1) (part : parts)
        Nothing -> do
          if partNumber == 1
          then do
            uploadUrl <- liftIO $ dieW (B2.get_upload_url env bucketID manager)
            liftIO $ dieW $ B2.upload_file uploadUrl filename bufferSize (sourceLazy $ BSB.toLazyByteString buffer) Nothing Nothing manager
          else do
            (part, fileID) <- uploadPart maybeFileID buffer bufferSize partNumber
            liftIO $ dieW $ finish_large_file env fileID (reverse (part : parts)) manager
    
    uploadPart :: Maybe LargeFile -> BSB.Builder -> Int64 -> Int64 -> ConduitT ByteString Void (ResourceT IO) (LargeFilePart, LargeFile)
    uploadPart Nothing buffer bufferSize partNumber = do
      fileID <- liftIO $ dieW (start_large_file env bucketID filename Nothing Nothing manager)
      uploadPart (Just fileID) buffer bufferSize partNumber
    uploadPart (Just fileID) buffer bufferSize partNumber = do
      url <- liftIO $ dieW $ B2.get_upload_part_url env fileID manager
      part <- liftIO $ dieW $ B2.upload_part url partNumber bufferSize (sourceLazy $ BSB.toLazyByteString buffer) manager
      return (part, fileID)

    handler :: Maybe LargeFile -> SomeException -> ConduitT ByteString Void (ResourceT IO) File
    handler Nothing exc = liftIO $ throwIO exc
    handler (Just fileID) exc = do
      _ <- liftIO $ dieW $ cancel_large_file env fileID manager
      handler Nothing exc

retry ::
  -- | Seconds
  [Double] ->
  IO a ->
  IO a
retry delaysSeconds io = loop delaysSeconds
  where
    loop [] = io
    loop (delay : delays) = catch io $ \(_ :: SomeException) -> do
      liftIO $ threadDelay (floor $ delay * 1000 * 1000)
      loop delays

retryThreeTimes :: IO a -> IO a
retryThreeTimes = retry [0, 0, 0]

dieW :: (Exception e) => IO (Either e a) -> IO a
dieW x = retryThreeTimes $ do
  res <- x
  either throwIO pure res
