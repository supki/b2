{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Cfg
  ( Cfg(..)
  , get
  , usageHeader
  ) where


import qualified B2
import           Env
import qualified Meta_b2_cli as Meta


data Cfg = Cfg
  { cfgKeyID          :: B2.ID B2.Key
  , cfgApplicationKey :: B2.ApplicationKey
  , cfgBaseUrl        :: B2.BaseUrl
  } deriving (Show, Eq)

get :: IO Cfg
get =
  Env.parse (header usageHeader) . prefixed "B2_" $ do
    cfgKeyID <-
      var str "KEY_ID" (help "Key ID")
    cfgApplicationKey <-
      var str "APPLICATION_KEY" (help "Application key")
    cfgBaseUrl <-
      var str "BASE_URL" (help "Base URL" <> def B2.defaultBaseUrl <> helpDef show)
    pure Cfg {..}

version :: String
version =
  Meta.version <> "-" <> Meta.hash

usageHeader :: String
usageHeader =
  unwords [Meta.name, version]
