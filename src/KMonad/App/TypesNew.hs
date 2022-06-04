-- NOTE: we will rename this module to Types when ready
module KMonad.App.TypesNew where

import KMonad.Prelude


data RunType = FullRun | CfgTest | EvTest
  deriving (Eq, Show)

data Cmd
  = SimpleCmd { _fullcmd :: Text}
  | CompoundCmd { _executable :: FilePath
                , _args :: [Text] }
    deriving (Eq, Show)
makeLenses ''Cmd


-------------------------------------------------------------------------------
-- I don't know where to put this stuff yet... Util?

