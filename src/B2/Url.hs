{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module B2.Url
  ( BaseUrl(..)
  , HasBaseUrl(..)
  , defaultBaseUrl
  , DownloadUrl(..)
  , HasDownloadUrl(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.String (IsString)


newtype BaseUrl = BaseUrl { unBaseUrl :: String }
    deriving (Show, Eq, IsString, Aeson.FromJSON)

class HasBaseUrl t where
  getBaseUrl :: t -> BaseUrl

instance HasBaseUrl BaseUrl where
  getBaseUrl x = x

defaultBaseUrl :: BaseUrl
defaultBaseUrl =
  "https://api.backblazeb2.com"

newtype DownloadUrl = DownloadUrl { unDownloadUrl :: String }
    deriving (Show, Eq, IsString, Aeson.FromJSON)

class HasDownloadUrl t where
  getDownloadUrl :: t -> DownloadUrl

instance HasDownloadUrl DownloadUrl where
  getDownloadUrl x = x
