{-# LANGUAGE RecordWildCards #-}
module App
  ( run
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy
import qualified Network.HTTP.Conduit as Http
import           System.Exit (exitFailure)
import           System.IO (stderr)

import           Cfg (Cfg(..))
import           Opts (Cmd(..))

import qualified B2


run :: Cfg -> Cmd -> IO ()
run Cfg {..} cmd = do
  man <- Http.newManager Http.tlsManagerSettings
  tokenE <- B2.b2_authorize_account B2.defaultBaseUrl cfgKeyID cfgApplicationKey man
  token <- dieE tokenE
  case cmd of
    CreateKey capabilities name durationS bucket prefix -> do
      keyE <- B2.b2_create_key token capabilities name durationS (fmap (\b -> (b, prefix)) bucket) man
      key <- dieE keyE
      print key
    ListKeys maxCount startKeyID -> do
      keysE <- B2.b2_list_keys token maxCount startKeyID man
      keys <- dieE keysE
      print keys

dieE :: Aeson.ToJSON e => Either e a -> IO a
dieE =
  either (\e -> do ByteString.Lazy.hPutStrLn stderr (Aeson.encode e); exitFailure) pure
