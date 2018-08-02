module Main
  ( main
  ) where

import qualified App
import qualified Cfg
import qualified Opts


main :: IO ()
main = do
  cfg <- Cfg.get
  cmd <- Opts.get
  App.run cfg cmd
