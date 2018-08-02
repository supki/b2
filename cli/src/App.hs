{-# LANGUAGE RecordWildCards #-}
module App
  ( run
  ) where

import qualified Network.HTTP.Conduit as Http
import           System.Exit (die)

import           Cfg (Cfg(..))
import           Opts (Cmd(..))

import qualified B2


run :: Cfg -> Cmd -> IO ()
run Cfg {..} cmd = do
  man <- Http.newManager Http.tlsManagerSettings
  tokenE <- B2.b2_authorize_account B2.defaultBaseUrl cfgKeyID cfgApplicationKey man
  flip (either (die . show)) tokenE $ \token ->
    case cmd of
      CreateKey capabilities name durationS bucket prefix -> do
        keyE <- B2.b2_create_key token capabilities name durationS (fmap (\b -> (b, prefix)) bucket) man
        flip (either (die . show)) keyE $ \key ->
          print key
