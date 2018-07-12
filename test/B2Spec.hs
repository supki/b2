{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
module B2Spec (spec) where

import           Data.Aeson.QQ (aesonQQ)
import qualified Data.Aeson as Aeson
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
        , bucketInfo=Aeson.Object mempty
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
            , bucketInfo=Aeson.Object mempty
            , bucketName="bucket01"
            , bucketType=AllPrivate
            , lifecycleRules=[]
            , revision=4
            }
          ])

  describe "b2_create_key" $
    it "parses success response" $
      pendingWith "Not implemented by B2"

  describe "b2_list_keys" $
    it "parses success response" $
      pendingWith "Not implemented by B2"
