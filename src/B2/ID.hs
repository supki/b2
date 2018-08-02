{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module B2.ID
  ( ID(..)
  , Account
  , HasAccountID(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.String (IsString)
import           Data.Text (Text)
import           Text.Printf (PrintfArg)


newtype ID tag = ID { unID :: Text }
    deriving         (Eq, IsString, PrintfArg, Aeson.FromJSON, Aeson.ToJSON)
    deriving newtype (Show)

data Account

class HasAccountID t where
  getAccountID :: t -> ID Account

instance account ~ Account => HasAccountID (ID account) where
  getAccountID x = x
