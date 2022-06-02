-- NOTE: we will rename this module to Types when ready
module KMonad.App.TypesNew where

import KMonad.Prelude


data File = FullPath FilePath | XdgCfgFile Text | Glob Text
  deriving (Eq, Show)

data RunType = FullRun | CfgTest | EvTest
  deriving (Eq, Show)

data Cmd
  = SimpleCmd { _fullcmd :: Text}
  | CompoundCmd { _executable :: FilePath
                , _args :: [Text] }
    deriving (Eq, Show)
makeLenses ''Cmd
