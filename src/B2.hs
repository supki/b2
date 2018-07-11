{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module B2
  ( module B2.AccountID
  , module B2.AuthorizationToken
  , module B2.BaseUrl
  , module B2.Bucket
  , module B2
  ) where

import           Control.Exception (Exception, throwIO)
import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import           Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import           Data.Int (Int64)
import           Data.Monoid ((<>))
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Conduit as Http
import qualified Network.HTTP.Types as Http

import           B2.AccountID
import           B2.AuthorizationToken
import           B2.BaseUrl
import           B2.Bucket


newtype KeyID = KeyID { unKeyID :: Text }
    deriving (Show, Eq, IsString)

newtype ApplicationKey = ApplicationKey { unApplicationKey :: Text }
    deriving (Show, Eq, IsString)

data AuthorizeAccount = AuthorizeAccount
  { accountID               :: AccountID
  , authorizationToken      :: AuthorizationToken
  , apiUrl                  :: BaseUrl
  , downloadUrl             :: Text
  , recommendedPartSize     :: Int64
  , absoluteMinimumPartSize :: Int64
  } deriving (Show, Eq)

instance Aeson.FromJSON AuthorizeAccount where
  parseJSON =
    Aeson.withObject "AuthorizeAccount" $ \o -> do
      accountID <- o .: "accountId"
      authorizationToken <- o .: "authorizationToken"
      apiUrl <- o .: "apiUrl"
      downloadUrl <- o .: "downloadUrl"
      recommendedPartSize <- o .: "recommendedPartSize"
      absoluteMinimumPartSize <- o .: "absoluteMinimumPartSize"
      pure AuthorizeAccount {..}

instance HasAccountID AuthorizeAccount where
  getAccountID AuthorizeAccount {..} = accountID

instance HasAuthorizationToken AuthorizeAccount where
  getAuthorizationToken AuthorizeAccount {..} = authorizationToken

instance HasBaseUrl AuthorizeAccount where
  getBaseUrl AuthorizeAccount {..} = apiUrl

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
  -> KeyID
  -> ApplicationKey
  -> Http.Manager
  -> IO (Either Error AuthorizeAccount)
b2_authorize_account url KeyID {unKeyID} ApplicationKey {unApplicationKey} man = do
  req <- generateRequest url "/b2api/v1/b2_authorize_account"
  res <- Http.httpLbs (applyAuth req) man
  parseResponse res
 where
  applyAuth =
    Http.applyBasicAuth (Text.encodeUtf8 unKeyID) (Text.encodeUtf8 unApplicationKey)

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
  req <- generateRequest env "/b2api/v1/b2_create_bucket"
  res <- Http.httpLbs req
    { Http.method="POST"
    , Http.requestHeaders=[authorization env]
    , Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
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

b2_delete_bucket
  :: ( Aeson.FromJSON info
     , Aeson.ToJSON info
     , HasBaseUrl env
     , HasAccountID env
     , HasAuthorizationToken env
     , HasBucketID bucketID
     )
  => env
  -> bucketID
  -> Http.Manager
  -> IO (Either Error (Bucket info))
b2_delete_bucket env id man = do
  req <- generateRequest env "/b2api/v1/b2_delete_bucket"
  res <- Http.httpLbs req
    { Http.method="POST"
    , Http.requestHeaders=[authorization env]
    , Http.requestBody=Http.RequestBodyLBS (Aeson.encode [aesonQQ|
        { accountId: #{getAccountID env}
        , bucketId: #{getBucketID id}
        }
      |])
    } man
  parseResponse res

generateRequest :: HasBaseUrl env => env -> String -> IO Http.Request
generateRequest url method =
  Http.parseRequest (unBaseUrl (getBaseUrl url) <> method)

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
  ("Authorization", value)
 where
  value =
    Text.encodeUtf8 (unAuthorizationToken (getAuthorizationToken env))
