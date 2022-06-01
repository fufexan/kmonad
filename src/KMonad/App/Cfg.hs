{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}

module KMonad.App.Cfg where

import KMonad.Prelude

import KMonad.Logging.Cfg
import KMonad.Keyboard.Types (DelayRate(..))

import System.IO

import Data.Either.Validation (Validation(..))
import GHC.Generics (Generic)

import qualified RIO.Text as T
import qualified Dhall as D
import qualified Dhall.Map as D

--------------------------------------------------------------------------------

data RunType = FullRun | ParseTest | EvTest

data CmdSpec
  = SimpleCmd { _fullcmd :: String}
  | CompoundCmd { _executable :: FilePath
                , _args :: [String] }
  | Pass
makeLenses ''CmdSpec


data FileSpec = FullPath FilePath | XdgCfgFile Text | Glob Text | Unspecified

data KeyInputCfg
  = LinEvdevSrc FileSpec
  | WinHookSrc
  | MacIOKitSrc (Maybe Text)
  | CmdSrc CmdSpec
  | NoKeyInput

data KeyOutputCfg
  = LinUinputSnk (Maybe Text)
  | WinSendSnk
  | MacKextSink
  | CmdSnk CmdSpec
  | NoKeyOutput

data KeyRepeatCfg
  = Simulate DelayRate
  | NoRepeat
  -- NOTE^: Here we can add 'detect repeat from OS then ping' idea from github

data  LocaleCfg = LocaleCfg
  { namedCodes :: [(Name, Natural)] -- ^ An alist of (name, keycode) pairs
  , namedGestures :: [(Name, Text)] -- ^ An alist of (name, gesture-string) pairs
  }

data RunCfg = RunCfg
  { cfgFile :: FileSpec
  , keymapFile :: FileSpec
  , cmdAllow :: Bool
  , runType :: RunType
  }

data KioCfg = KioCfg
  { keyRepeatCfg :: KeyRepeatCfg
  , fallthrough :: Bool
  , keyInputCfg :: KeyInputCfg
  , keyOutputCfg :: KeyOutputCfg
  , preKIOcmd :: CmdSpec
  , postKIOcmd :: CmdSpec
  }

data AppCfg = AppCfg
  { appLocaleCfg :: LocaleCfg
  , appLogCfg :: LogCfg
  , appKioCfg :: KioCfg
  , appRunCfg :: RunCfg
  }


defCfg :: AppCfg
defCfg = AppCfg
  { appLocaleCfg = LocaleCfg
    { namedCodes    = [("a", 1), ("b", 2)]
    , namedGestures = [("A", "S-a"), ("^", "S-6")]
    }
  , appLogCfg = LogCfg
    { _logLevel = LevelWarn
    }
  , appKioCfg = KioCfg
    { keyRepeatCfg = Simulate (DelayRate 300 100)
    , fallthrough = True
    , keyInputCfg = NoKeyInput
    , keyOutputCfg = NoKeyOutput
    , preKIOcmd = Pass
    , postKIOcmd = Pass
    }
  , appRunCfg = RunCfg
    { cfgFile = XdgCfgFile "config.dhall"
    , keymapFile = XdgCfgFile "keymap.kbd"
    , cmdAllow = False
    , runType = FullRun
    }
  }

data DEntry k v = DEntry
  { mapKey :: k
  , mapValue :: v
  } deriving (Generic, D.FromDhall, Show)

-- | The settings that we want to expose to Dhall
--
-- This explicitly leaves out:
-- cfgFile: because it would point at self
-- runType: because it can only be provided by Invoc
data DhallCfg = DhallCfg
  { dcodeNames :: [DEntry Name Natural]
  , dgestureNames :: [DEntry Name Text]
  , dlogLevel :: Text
  , dkeyRepeat :: Text
  , dfallthrough :: Bool
  , dkeyInputCfg :: Text
  , dkeyOutputCfg :: Text
  , dpreKIOcmd :: Text
  , dpostKIOcmd :: Text
  , dkeymapFile :: Text
  , dcmdAllow :: Bool
  } deriving (Generic, D.FromDhall, Show)


loadDhallCfg :: MonadIO m => FilePath -> m DhallCfg
loadDhallCfg f = do
  let opt = D.defaultInterpretOptions { D.fieldModifier = T.drop 1 }
  let dec = D.genericAutoWith opt
  liftIO $ D.inputFile dec f


main :: IO DhallCfg
main = loadDhallCfg "/home/david/prj/kmonad/cfg/linux.dhall"
