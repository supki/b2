{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module B2.AuthorizationToken
  ( HasAuthorizationToken(..)
  , AuthorizationToken(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.String (IsString)
import           Data.Text (Text)


newtype AuthorizationToken = AuthorizationToken { unAuthorizationToken :: Text }
    deriving (Show, Eq, IsString, Aeson.FromJSON)

class HasAuthorizationToken t where
  getAuthorizationToken :: t -> AuthorizationToken

instance HasAuthorizationToken AuthorizationToken where
  getAuthorizationToken x = x
