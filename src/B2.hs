{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module B2
  ( module B2.AuthorizationToken
  , module B2.Bucket
  , module B2.File
  , module B2.ID
  , module B2.Key
  , module B2.Upload
  , module B2.Url
  , module B2
  ) where

import           Control.Exception (Exception, throwIO)
import           Control.Monad (join)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Crypto.Hash as Hash
import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import           Data.Aeson.QQ (aesonQQ)
import           Data.Bifunctor (bimap)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import           Data.Conduit (ConduitT)
import           Data.Int (Int64)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.String (fromString)
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

data Ex
  = JsonEx Lazy.ByteString String
    deriving (Show, Eq)

instance Exception Ex

b2_authorize_account
  :: HasBaseUrl url
  => url
  -> ID Key
  -> ApplicationKey
  -> Http.Manager
  -> IO (Either Error AuthorizeAccount)
b2_authorize_account url keyID applicationKey man = do
  req <- basicRequest url keyID applicationKey "/b2api/v1/b2_authorize_account"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS "{}"
    } man
  parseResponse res

b2_create_bucket
  :: ( Aeson.FromJSON info
     , Aeson.ToJSON info
     , HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     )
  => env
  -> Text
  -> BucketType
  -> Maybe info
  -> Maybe [CorsRule]
  -> Maybe [LifecycleRule]
  -> Http.Manager
  -> IO (Either Error (Bucket info))
b2_create_bucket env name type_ info cors lifecycle man = do
  req <- tokenRequest env "/b2api/v1/b2_create_bucket"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { accountId: #{getAccountID env}
        , bucketName: #{name}
        , bucketType: #{type_}
        , bucketInfo: #{info}
        , corsRules: #{cors}
        , lifecycleRules: #{lifecycle}
        }
      |])
    } man
  parseResponse res

b2_list_buckets
  :: ( Aeson.FromJSON info
     , HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     )
  => env
  -> Maybe (ID Bucket)
  -> Maybe Text
  -> Maybe [BucketType]
  -> Http.Manager
  -> IO (Either Error [Bucket info])
b2_list_buckets env bucket name types man = do
  req <- tokenRequest env "/b2api/v1/b2_list_buckets"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { accountId: #{getAccountID env}
        , bucketId: #{bucket}
        , bucketName: #{name}
        , bucketTypes: #{types}
        }
      |])
    } man
  fmap (fmap unBuckets) (parseResponse res)

b2_update_bucket
  :: ( HasBucketID bucketID
     , Aeson.FromJSON info
     , Aeson.ToJSON info
     , HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Maybe BucketType
  -> Maybe info
  -> Maybe [CorsRule]
  -> Maybe [LifecycleRule]
  -> Maybe Int64
  -> Http.Manager
  -> IO (Either Error (Bucket info))
b2_update_bucket env bucket type_ info cors lifecycle revision man = do
  req <- tokenRequest env "/b2api/v1/b2_update_bucket"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { accountId: #{getAccountID env}
        , bucketId: #{getBucketID bucket}
        , bucketType: #{type_}
        , bucketInfo: #{info}
        , corsRules: #{cors}
        , lifecycleRules: #{lifecycle}
        , ifRevisionIs: #{revision}
        }
      |])
    } man
  parseResponse res

b2_delete_bucket
  :: ( Aeson.FromJSON info
     , HasBucketID bucketID
     , HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Http.Manager
  -> IO (Either Error (Bucket info))
b2_delete_bucket env id man = do
  req <- tokenRequest env "/b2api/v1/b2_delete_bucket"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { accountId: #{getAccountID env}
        , bucketId: #{getBucketID id}
        }
      |])
    } man
  parseResponse res

b2_create_key
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
b2_create_key env capabilities name durationS restrictions man = do
  req <- tokenRequest env "/b2api/v1/b2_create_key"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { accountId: #{getAccountID env}
        , capabilities: #{capabilities}
        , keyName: #{name}
        , validDurationInSeconds: #{durationS}
        , bucketId: #{fmap fst restrictions}
        , namePrefix: #{join (fmap snd restrictions)}
        }
      |])
    } man
  parseResponse res

b2_list_keys
  :: ( HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     )
  => env
  -> Maybe Int64
  -> Maybe (ID Key)
  -> Http.Manager
  -> IO (Either Error Keys)
b2_list_keys env maxKeyCount startApplicationKeyID man = do
  req <- tokenRequest env "/b2api/v1/b2_list_keys"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { accountId: #{getAccountID env}
        , maxKeyCount: #{maxKeyCount}
        , startApplicationKeyId: #{startApplicationKeyID}
        }
      |])
    } man
  parseResponse res

b2_delete_key
  :: ( HasKeyID keyID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> keyID
  -> Http.Manager
  -> IO (Either Error (Key NoSecret))
b2_delete_key env id man = do
  req <- tokenRequest env "/b2api/v1/b2_delete_key"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { applicationKeyId: #{getKeyID id}
        }
      |])
    } man
  parseResponse res

b2_get_upload_url
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Http.Manager
  -> IO (Either Error UploadInfo)
b2_get_upload_url env id man = do
  req <- tokenRequest env "/b2api/v1/b2_get_upload_url"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { bucketId: #{getBucketID id}
        }
      |])
    } man
  parseResponse res

b2_upload_file
  :: ( HasUploadUrl env
     , HasAuthorizationToken env
     )
  => env
  -> Text
  -> Maybe Text
  -> Lazy.ByteString
  -> [(Http.HeaderName, Text)]
  -> Http.Manager
  -> IO (Either Error File)
b2_upload_file env name contentType content info man = do
  req <- uploadRequest env name content contentType info
  res <- Http.httpLbs req man
  parseResponse res

b2_delete_file_version
  :: ( HasFileID fileID
     , HasFileName fileName
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> fileID
  -> fileName
  -> Http.Manager
  -> IO (Either Error FileIDs)
b2_delete_file_version env id name man = do
  req <- tokenRequest env "/b2api/v1/b2_delete_file_version"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { fileId: #{getFileID id}
        , fileName: #{getFileName name}
        }
      |])
    } man
  parseResponse res

b2_get_file_info
  :: ( HasFileID fileID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> fileID
  -> Http.Manager
  -> IO (Either Error File)
b2_get_file_info env id man = do
  req <- tokenRequest env "/b2api/v1/b2_get_file_info"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { fileId: #{getFileID id}
        }
      |])
    } man
  parseResponse res

b2_list_file_names
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Maybe Text
  -> Maybe Int64
  -> Maybe Text
  -> Maybe Text
  -> Http.Manager
  -> IO (Either Error Files)
b2_list_file_names env id startName maxCount prefix delimiter man = do
  req <- tokenRequest env "/b2api/v1/b2_list_file_names"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { bucketId: #{getBucketID id}
        , startFileName: #{startName}
        , maxFileCount: #{maxCount}
        , prefix: #{prefix}
        , delimiter: #{delimiter}
        }
      |])
    } man
  parseResponse res

b2_list_file_versions
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Maybe (Text, Maybe (ID File))
  -> Maybe Int64
  -> Maybe Text
  -> Maybe Text
  -> Http.Manager
  -> IO (Either Error Files)
b2_list_file_versions env id startName maxCount prefix delimiter man = do
  req <- tokenRequest env "/b2api/v1/b2_list_file_versions"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { bucketId: #{getBucketID id}
        , startFileName: #{startName}
        , startFileId: #{join (fmap snd startName)}
        , maxFileCount: #{maxCount}
        , prefix: #{prefix}
        , delimiter: #{delimiter}
        }
      |])
    } man
  parseResponse res

b2_download_file_by_name
  :: ( HasDownloadUrl env
     , HasAuthorizationToken env
     , MonadResource m
     )
  => env
  -> (Maybe Int64, Maybe Int64)
  -> Text
  -> Text
  -> Http.Manager
  -> m (ConduitT i ByteString m ())
b2_download_file_by_name env range bucketName fileName man = do
  req <- downloadByNameRequest env range bucketName fileName
  res <- Http.http req man
  pure (Http.responseBody res)

b2_download_file_by_id
  :: ( HasFileID fileID
     , HasDownloadUrl env
     , HasAuthorizationToken env
     , MonadResource m
     )
  => env
  -> (Maybe Int64, Maybe Int64)
  -> fileID
  -> Http.Manager
  -> m (ConduitT i ByteString m ())
b2_download_file_by_id env range fileID man = do
  req <- downloadByIDRequest env range fileID
  res <- Http.http req man
  pure (Http.responseBody res)

b2_get_download_authorization
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
b2_get_download_authorization env bucket fileNamePrefix durationS disposition man = do
  req <- tokenRequest env "/b2api/v1/b2_get_download_authorization"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { bucketId: #{getBucketID bucket}
        , fileNamePrefix: #{fileNamePrefix}
        , validDurationInSeconds: #{durationS}
        , b2ContentDisposition: #{disposition}
        }
      |])
    } man
  parseResponse res

b2_hide_file
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Text
  -> Http.Manager
  -> IO (Either Error DownloadAuthorization)
b2_hide_file env bucket fileName man = do
  req <- tokenRequest env "/b2api/v1/b2_hide_file"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { bucketId: #{getBucketID bucket}
        , fileName: #{fileName}
        }
      |])
    } man
  parseResponse res

b2_get_upload_part_url
  :: ( HasFileID fileID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> fileID
  -> Http.Manager
  -> IO (Either Error UploadPartInfo)
b2_get_upload_part_url env id man = do
  req <- tokenRequest env "/b2api/v1/b2_get_upload_part_url"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { fileId: #{getFileID id}
        }
      |])
    } man
  parseResponse res

b2_start_large_file
  :: ( HasBucketID bucketID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> bucketID
  -> Text
  -> Text
  -> Maybe [(Text, Text)]
  -> Http.Manager
  -> IO (Either Error LargeFile)
b2_start_large_file env bucket fileName contentType info man = do
  req <- tokenRequest env "/b2api/v1/b2_start_large_file"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { bucketId: #{getBucketID bucket}
        , fileName: #{fileName}
        , contentType: #{contentType}
        , fileInfo: #{fmap HashMap.fromList info}
        }
      |])
    } man
  parseResponse res

b2_list_unfinished_large_files
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
b2_list_unfinished_large_files env bucket namePrefix startFileID maxCount man = do
  req <- tokenRequest env "/b2api/v1/b2_list_unfinished_large_files"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { bucketId: #{getBucketID bucket}
        , namePrefix: #{namePrefix}
        , startFileId: #{startFileID}
        , maxFileCount: #{maxCount}
        }
      |])
    } man
  parseResponse res

b2_cancel_large_file
  :: ( HasFileID fileID
     , HasBaseUrl env
     , HasAuthorizationToken env
     )
  => env
  -> fileID
  -> Http.Manager
  -> IO (Either Error FileIDs)
b2_cancel_large_file env file man = do
  req <- tokenRequest env "/b2api/v1/b2_cancel_large_file"
  res <- Http.httpLbs req
    { Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { fileId: #{getFileID file}
        }
      |])
    } man
  parseResponse res

b2_upload_part
  :: ( HasUploadPartUrl env
     , HasAuthorizationToken env
     )
  => env
  -> Int64
  -> ByteString
  -> Http.Manager
  -> IO (Either Error LargeFilePart)
b2_upload_part env idx part man = do
  req <- uploadPartRequest env idx part
  res <- Http.httpLbs req man
  parseResponse res

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
  -> Lazy.ByteString
  -> Maybe Text
  -> [(Http.HeaderName, Text)]
  -> IO Http.Request
uploadRequest env name content contentType info = do
  req <- Http.parseRequest (unUploadUrl (getUploadUrl env))
  pure req
    { Http.method="POST"
    , Http.requestHeaders=
      ( authorization env
      : ("X-Bz-File-Name", urlEncode (Text.encodeUtf8 name))
      : ("Content-Type", maybe "b2/x-auto" Text.encodeUtf8 contentType)
      : ("X-Bz-Content-Sha1", "do_not_verify")
      : map (bimap ("X-Bz-Info-" <>) (urlEncode . Text.encodeUtf8)) info
      )
    , Http.requestBody=Http.RequestBodyLBS content
    }
 where
  urlEncode =
    Http.urlEncode True

uploadPartRequest
  :: (HasUploadPartUrl env, HasAuthorizationToken env)
  => env
  -> Int64
  -> ByteString
  -> IO Http.Request
uploadPartRequest env idx content = do
  req <- Http.parseRequest (unUploadPartUrl (getUploadPartUrl env))
  pure req
    { Http.method="POST"
    , Http.requestHeaders=
      ( authorization env
      : ("X-Bz-Part-Number", text idx)
      : ("Content-Length", text (ByteString.length content))
      : ("X-Bz-Content-Sha1", text (Hash.hashWith Hash.SHA1 content))
      : []
      )
    , Http.requestBody=Http.RequestBodyBS content
    }
 where
  text :: Show a => a -> ByteString
  text =
    fromString . show

downloadByNameRequest
  :: ( HasDownloadUrl env
     , HasAuthorizationToken env
     , MonadIO m
     )
  => env
  -> (Maybe Int64, Maybe Int64)
  -> Text
  -> Text
  -> m Http.Request
downloadByNameRequest env range bucket file =
  downloadRequest env range (printf "/file/%s/%s" bucket file)

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

parseResponse
  :: (Aeson.FromJSON err, Aeson.FromJSON a)
  => Http.Response Lazy.ByteString
  -> IO (Either err a)
parseResponse res = do
  if 200 <= statusCode && statusCode < 300
    then fmap Right (parseJsonEx body)
    else fmap Left (parseJsonEx body)
 where
  body = Http.responseBody res
  Http.Status {statusCode} = Http.responseStatus res

parseJsonEx :: Aeson.FromJSON a => Lazy.ByteString -> IO a
parseJsonEx bytes =
  either (throwIO . JsonEx bytes) pure (Aeson.eitherDecode bytes)

authorization :: HasAuthorizationToken env => env -> Http.Header
authorization env =
  ("Authorization", Text.encodeUtf8 (unAuthorizationToken (getAuthorizationToken env)))
