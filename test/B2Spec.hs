{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
module B2Spec (spec) where

import           Data.Aeson.QQ (aesonQQ)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import           Prelude hiding (all)
import           Test.Hspec

import           B2


spec :: Spec
spec = parallel $ do
  it "parses failure response" $
    Aeson.decode (Aeson.encode ([aesonQQ|
      { code: "bad_auth_token"
      , message: "Invalid authorization token"
      , status: 401
      }
    |])) `shouldBe` pure Error
      { code="bad_auth_token"
      , message="Invalid authorization token"
      , status=401
      }

  describe "b2_authorize_account" $
    it "parses success response" $
      Aeson.decode (Aeson.encode ([aesonQQ|
        { absoluteMinimumPartSize: 5000000
        , accountId: "..."
        , allowed:
          { bucketId: null
          , capabilities: ["all"]
          , namePrefix: null
          }
        , apiUrl: "https://api002.backblazeb2.com"
        , authorizationToken: "..."
        , downloadUrl: "https://f002.backblazeb2.com"
        , minimumPartSize: 100000000
        , recommendedPartSize: 100000000
        }
      |])) `shouldBe` pure AuthorizeAccount
        { accountID="..."
        , authorizationToken="..."
        , allowed=Allowed
          { bucketID=Nothing
          , capabilities=["all"]
          , namePrefix=Nothing
          }
        , apiUrl="https://api002.backblazeb2.com"
        , downloadUrl="https://f002.backblazeb2.com"
        , recommendedPartSize=100000000
        , absoluteMinimumPartSize=5000000
        }

  describe "b2_create_bucket" $
    it "parses success response" $
      Aeson.decode (Aeson.encode ([aesonQQ|
        { accountId: "..."
        , bucketId: "843f1470e5fe80a2684a0715"
        , bucketInfo: {}
        , bucketName: "bucket01"
        , bucketType: "allPrivate"
        , corsRules: []
        , lifecycleRules: []
        , revision: 4
        }
      |])) `shouldBe` pure Bucket
        { accountID="..."
        , bucketID="843f1470e5fe80a2684a0715"
        , bucketInfo=mempty
        , bucketName="bucket01"
        , bucketType=AllPrivate
        , lifecycleRules=[]
        , revision=4
        }

  describe "b2_list_buckets" $
    it "parses success response" $
      Aeson.decode (Aeson.encode ([aesonQQ|
        { buckets:
          [ { accountId: "..."
            , bucketId: "843f1470e5fe80a2684a0715"
            , bucketInfo: {}
            , bucketName: "bucket01"
            , bucketType: "allPrivate"
            , corsRules: []
            , lifecycleRules: []
            , revision: 4
            }
          ]
        }
      |])) `shouldBe` pure
        (Buckets
          [ Bucket
            { accountID="..."
            , bucketID="843f1470e5fe80a2684a0715"
            , bucketInfo=mempty
            , bucketName="bucket01"
            , bucketType=AllPrivate
            , lifecycleRules=[]
            , revision=4
            }
          ])

  describe "b2_create_key" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { accountId: "..."
        , applicationKeyId: "01020304"
        , applicationKey: "secret"
        , capabilities: ["all"]
        , bucketId: null
        , expirationTimestamp: null
        , keyName: "key01"
        , namePrefix: null
        }
      |])) `shouldBe` pure Key
        { applicationKeyID="01020304"
        , applicationKey="secret" :: ApplicationKey
        , capabilities=[all]
        , accountID="..."
        , bucketID=Nothing
        , expirationTimestampMS=Nothing
        , keyName="key01"
        , namePrefix=Nothing
        }

  describe "b2_list_keys" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { keys:
          [ { accountId: "..."
            , applicationKeyId: "01020304"
            , capabilities: ["all"]
            , bucketId: null
            , expirationTimestamp: null
            , keyName: "key01"
            , namePrefix: null
            }
          ]
        , nextApplicationKeyId: "01020305"
        }
      |])) `shouldBe` pure Keys
        { keys=
          [ Key
            { applicationKeyID="01020304"
            , applicationKey=NoSecret
            , capabilities=[all]
            , accountID="..."
            , bucketID=Nothing
            , expirationTimestampMS=Nothing
            , keyName="key01"
            , namePrefix=Nothing
            }
          ]
        , nextApplicationKeyID=pure "01020305"
        }

  describe "b2_delete_key" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { accountId: "..."
        , applicationKeyId: "01020304"
        , capabilities: ["all"]
        , bucketId: null
        , expirationTimestamp: null
        , keyName: "key01"
        , namePrefix: null
        }
      |])) `shouldBe` pure Key
        { applicationKeyID="01020304"
        , applicationKey=NoSecret
        , capabilities=[all]
        , accountID="..."
        , bucketID=Nothing
        , expirationTimestampMS=Nothing
        , keyName="key01"
        , namePrefix=Nothing
        }

  describe "b2_get_upload_url" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { authorizationToken: "..."
        , bucketId: "041fc46015ee80d2684a0715"
        , uploadUrl: "https://pod-....backblaze.com/b2api/v1/b2_upload_file/..."
        }
      |])) `shouldBe` pure UploadInfo
        { bucketID="041fc46015ee80d2684a0715"
        , uploadUrl="https://pod-....backblaze.com/b2api/v1/b2_upload_file/..."
        , authorizationToken="..."
        }

  describe "b2_upload_file" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { accountId: "..."
        , action: "upload"
        , bucketId: "041fc46015ee80d2684a0715"
        , contentLength: 13293
        , contentSha1: "unverified:5b13b936d24e0ea47940e84d3021866a05887688"
        , contentType: "text/plain"
        , fileId: "..."
        , fileInfo:
          { test: "value"
          }
        , fileName: ".vimrc"
        , uploadTimestamp: 1531422158000
        }
      |])) `shouldBe` pure File
        { contentLength=13293
        , contentSha1="unverified:5b13b936d24e0ea47940e84d3021866a05887688"
        , contentType="text/plain"
        , fileIDs=FileIDs
          { fileID="..."
          , fileName=".vimrc"
          }
        , fileInfo=HashMap.singleton "test" "value"
        , action="upload"
        , uploadTimestamp=1531422158000
        }

  describe "b2_delete_file_version" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { fileId: "..."
        , fileName: ".vimrc"
        }
      |])) `shouldBe` pure FileIDs
        { fileID="..."
        , fileName=".vimrc"
        }

  describe "b2_list_file_names" $
    it "parses success response" $ do
      Aeson.eitherDecode (Aeson.encode ([aesonQQ|
        { files:
          [ { action: "upload"
            , contentLength: 13293
            , contentSha1: "unverified:5b13b936d24e0ea47940e84d3021866a05887688"
            , contentType: "text/plain"
            , fileId: "..."
            , fileInfo:
                { test: "value"
                }
            , fileName: ".vimrc"
            , size: 13293
            , uploadTimestamp: 1531423823000
            }
          ]
        , nextFileName: null
        }
      |])) `shouldBe` pure Files
        { files=
          [ File
            { contentLength=13293
            , contentSha1="unverified:5b13b936d24e0ea47940e84d3021866a05887688"
            , contentType="text/plain"
            , fileIDs=FileIDs
              { fileID="..."
              , fileName=".vimrc"
              }
            , fileInfo=HashMap.singleton "test" "value"
            , action="upload"
            , uploadTimestamp=1531423823000
            }
          ]
        , nextFileName=Nothing
        , nextFileId=Nothing
        }

  describe "b2_get_download_authorization" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { authorizationToken: "..."
        , bucketId: "041fc46015ee80d2684a0715"
        , fileNamePrefix: ""
        }
      |])) `shouldBe` pure DownloadAuthorization
        { authorizationToken="..."
        , bucketID= "041fc46015ee80d2684a0715"
        , fileNamePrefix=""
        }

  describe "b2_hide_file" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { action: "hide"
        , contentLength: 0
        , contentSha1: "da39a3ee5e6b4b0d3255bfef95601890afd80709"
        , contentType: "application/x-bz-hide-marker"
        , fileId: "..."
        , fileInfo: {}
        , fileName: ".vimrc"
        , size: 0
        , uploadTimestamp: 1531574875000
        }
      |])) `shouldBe` pure File
        { contentLength=0
        , contentSha1="da39a3ee5e6b4b0d3255bfef95601890afd80709"
        , contentType="application/x-bz-hide-marker"
        , fileIDs=FileIDs
          { fileID="..."
          , fileName=".vimrc"
          }
        , fileInfo=HashMap.empty
        , action="hide"
        , uploadTimestamp=1531574875000
        }

  describe "b2_start_large_file" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { accountId: "..."
        , bucketId: "041fc46015ee80d2684a0715"
        , contentType: "text/plain"
        , fileId: "..."
        , fileInfo: {}
        , fileName: ".vimrc"
        , uploadTimestamp: 1531637809000
        }
      |])) `shouldBe` pure LargeFile
        { contentType="text/plain"
        , fileIDs=FileIDs
          { fileID="..."
          , fileName=".vimrc"
          }
        , fileInfo=HashMap.empty
        , uploadTimestamp=1531637809000
        }

  describe "b2_list_unfinished_large_files" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { files:
          [ { accountId: "..."
            , bucketId: "041fc46015ee80d2684a0715"
            , contentType: "text/plain"
            , fileId: "..."
            , fileInfo: {}
            , fileName: ".vimrc"
            , uploadTimestamp: 1531637809000
            }
          ]
        , nextFileId: null
        }
      |])) `shouldBe` pure LargeFiles
        { files=
          [ LargeFile
            { contentType="text/plain"
            , fileIDs=FileIDs
              { fileID="..."
              , fileName=".vimrc"
              }
            , fileInfo=HashMap.empty
            , uploadTimestamp=1531637809000
            }
          ]
        , nextFileID=Nothing
        }

  describe "b2_upload_part" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { fileId: "..."
        , contentLength: 4
        , contentSha1: "f68f58a2791058311196bfcbb46f2f98038ddac2"
        , partNumber: 1
        }
      |])) `shouldBe` pure LargeFilePart
        { fileID="..."
        , partNumber=1
        , contentLength=4
        , contentSha1="f68f58a2791058311196bfcbb46f2f98038ddac2"
        }

  describe "b2_list_parts" $
    it "parses success response" $ do
      Aeson.decode (Aeson.encode ([aesonQQ|
        { nextPartNumber: null
        , parts:
          [ { contentLength: 4
            , contentSha1: "f68f58a2791058311196bfcbb46f2f98038ddac2"
            , fileId: "..."
            , partNumber: 1
            , uploadTimestamp: 1531847978000
            }
          ]
        }
      |])) `shouldBe` pure LargeFileParts
        { nextPartNumber=Nothing
        , parts=
          [ LargeFilePart
            { fileID="..."
            , partNumber=1
            , contentLength=4
            , contentSha1="f68f58a2791058311196bfcbb46f2f98038ddac2"
            }
          ]
        }
