{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module B2.BaseUrl
  ( HasBaseUrl(..)
  , BaseUrl(..)
  , defaultBaseUrl
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
