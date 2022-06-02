{-# LANGUAGE DeriveAnyClass #-}
module KMonad.App.Cfg.Types where


import KMonad.Prelude

import KMonad.App.TypesNew

import KMonad.Logging.Cfg
import KMonad.Keyboard.Types (DelayRate(..))

import System.IO

import Data.Either.Validation (Validation(..))
import GHC.Generics (Generic)

import qualified RIO.Text as T
import qualified Dhall as D
import qualified Dhall.Map as D

--------------------------------------------------------------------------------

type CmdSpec = Text
type FileSpec = Text
type KeyInputSpec = Text
type KeyOutputSpec = Text
type KeyRepeatSpec = Text
type LogLevelSpec = Text

 -------------------------------------------------------------------------------

data KeyInputCfg
  = LinEvdevSrc File
  | WinHookSrc
  | MacIOKitSrc (Maybe Text)
  | CmdSrc Cmd
  | StdinSrc
  deriving (Eq, Show)

data KeyOutputCfg
  = LinUinputSnk (Maybe Text)
  | WinSendSnk
  | MacKextSink
  | CmdSnk Cmd
  | StdoutSnk
  deriving (Eq, Show)

data KeyRepeatCfg
  = Simulate DelayRate
  | EchoOS
  deriving (Eq, Show)
  -- NOTE^: Here we can add 'detect repeat from OS then ping' idea from github

data  LocaleCfg = LocaleCfg
  { namedCodes :: [(Name, Natural)] -- ^ An alist of (name, keycode) pairs
  , namedGestures :: [(Name, Text)] -- ^ An alist of (name, gesture-string) pairs
  }

data RunCfg = RunCfg
  { cfgFile :: File
  , keymapFile :: File
  , cmdAllow :: Bool
  , runType :: RunType
  }

data KioCfg = KioCfg
  { keyRepeatCfg :: KeyRepeatCfg
  , fallthrough  :: Bool
  , keyInputCfg  :: Maybe KeyInputCfg
  , keyOutputCfg :: Maybe KeyOutputCfg
  , preKIOcmd    :: Maybe Cmd
  , postKIOcmd   :: Maybe Cmd
  }

data AppCfg = AppCfg
  { appLocaleCfg :: LocaleCfg
  , appLogCfg :: LogCfg
  , appKioCfg :: KioCfg
  , appRunCfg :: RunCfg
  }

-- invoc  ----------------------------------------------------------------------

data Invoc = Invoc
  { irunType :: RunType
  , icfgFile :: Maybe FileSpec
  , ikeymapFile :: Maybe FileSpec
  , ifallthrough :: Maybe Bool
  , icmdAllow :: Maybe Bool
  , ilogLevel :: Maybe LogLevelSpec
  , ikeyRepeat :: Maybe KeyRepeatSpec
  , ikeyInputCfg :: Maybe KeyInputSpec
  , ikeyOutputCfg :: Maybe KeyOutputSpec
  , ipreKIOcmd :: Maybe CmdSpec
  , ipostKIOcmd :: Maybe CmdSpec
  } deriving (Eq, Show)

-- dhall -----------------------------------------------------------------------

data DEntry k v = DEntry
  { mapKey :: k
  , mapValue :: v
  } deriving (Generic, D.FromDhall, Show)

-- | The settings that we want to expose to Dhall
--
-- This explicitly leaves out:
-- cfgFile: because it would point at self
-- runType: because it can only be provided by Invoc
--
-- NOTE: the difference between this and 'Invoc', here the only time we use
-- 'Maybe' is to denote the setting of not-doing-something. In 'Invoc' 'Nothing'
-- denotes do-not-change-this-setting. This is because we encode our app
-- defaults *in dhall*. So the default invoc settings are to change nothing, the
-- default CfgFile settings *are* the app defaults.
data DhallCfg = DhallCfg
  { dcodeNames    :: [DEntry Name Natural]
  , dgestureNames :: [DEntry Name Text]
  , dfallthrough  :: Bool
  , dcmdAllow     :: Bool
  , dlogLevel     :: LogLevelSpec
  , dkeyInputCfg  :: KeyInputSpec
  , dkeyOutputCfg :: KeyOutputSpec
  , dkeymapFile   :: FileSpec
  , dkeyRepeat    :: Maybe KeyRepeatSpec
  , dpreKIOcmd    :: Maybe CmdSpec
  , dpostKIOcmd   :: Maybe CmdSpec
  } deriving (Generic, D.FromDhall, Show)


loadDhallCfg :: MonadIO m => FilePath -> m DhallCfg
loadDhallCfg f = do
  let opt = D.defaultInterpretOptions { D.fieldModifier = T.drop 1 }
  let dec = D.genericAutoWith opt
  liftIO $ D.inputFile dec f


testDhall :: IO ()
testDhall = pPrint =<< loadDhallCfg "/home/david/prj/kmonad/cfg/linux.dhall"
