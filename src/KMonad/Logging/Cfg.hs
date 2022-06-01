module KMonad.Logging.Cfg where

import KMonad.Prelude

import Dhall (FromDhall, ToDhall)


newtype LogCfg = LogCfg { _logLevel :: LogLevel}
