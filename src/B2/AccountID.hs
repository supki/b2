{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module B2.AccountID
  ( AccountID(..)
  , HasAccountID(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.String (IsString)
import           Data.Text (Text)


newtype AccountID = AccountID { unAccountID :: Text }
    deriving (Show, Eq, IsString, Aeson.FromJSON, Aeson.ToJSON)

class HasAccountID t where
  getAccountID :: t -> AccountID

instance HasAccountID AccountID where
  getAccountID x = x
