{-# LANGUAGE DeriveAnyClass #-}
module KMonad.App.Cfg.Types where


import KMonad.Prelude

import KMonad.App.TypesNew

import KMonad.Gesture
import KMonad.Logging.Cfg
import KMonad.Keyboard.Types (DelayRate(..))

import System.IO

import Data.Either.Validation (Validation(..))
import GHC.Generics (Generic)

import qualified RIO.HashMap as M
import qualified RIO.Text as T
import qualified Dhall as D

--------------------------------------------------------------------------------

type CmdSpec = Text
type FileSpec = Text
type KeyInputSpec = Text
type KeyOutputSpec = Text
type KeyRepeatSpec = Text
type LogLevelSpec = Text
type GestureExpr = Text

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
  { namedCodes :: M.HashMap Name Natural
  , namedGestures :: M.HashMap Name (Gesture Natural)
  } deriving (Eq, Show)

data RunCfg = RunCfg
  { cfgFile :: File
  , keymapFile :: File
  , cmdAllow :: Bool
  , runType :: RunType
  } deriving (Eq, Show)

data KioCfg = KioCfg
  { keyRepeatCfg :: KeyRepeatCfg
  , fallthrough  :: Bool
  , keyInputCfg  :: KeyInputCfg
  , keyOutputCfg :: KeyOutputCfg
  , preKIOcmd    :: Maybe Cmd
  , postKIOcmd   :: Maybe Cmd
  } deriving (Eq, Show)

data AppCfg = AppCfg
  { appLocaleCfg :: LocaleCfg
  , appLogCfg :: LogCfg
  , appKioCfg :: KioCfg
  , appRunCfg :: RunCfg
  } deriving (Eq, Show)

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

defCfgFile :: FileSpec
defCfgFile = "cfg:kmonad.dhall"

-- dhall -----------------------------------------------------------------------

data DEntry k v = DEntry
  { _mapKey :: k
  , _mapValue :: v
  } deriving (Generic, D.FromDhall, Show)
makeLenses ''DEntry

type DMap k v= [DEntry k v]

-- _Tuple :: Iso' (DEntry k v) (k, v)
-- _Tuple = iso (\e -> (e^.mapKey, e^.mapValue)) $ uncurry DEntry


-- | Use '_DMap' as a view of an alist as a DMap, and 'from _DMap' as its inverse
_DMap :: Iso' [(k, v)] (DMap k v)
_DMap = iso (map $ uncurry DEntry) (map $ view mapKey &&& view mapValue)

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
  { _dcodeNames    :: [DEntry Name Natural]
  , _dgestureNames :: [DEntry Name GestureExpr]
  , _dfallthrough  :: Bool
  , _dcmdAllow     :: Bool
  , _dlogLevel     :: LogLevelSpec
  , _dkeyInputCfg  :: KeyInputSpec
  , _dkeyOutputCfg :: KeyOutputSpec
  , _dkeymapFile   :: FileSpec
  , _dkeyRepeat    :: Maybe KeyRepeatSpec
  , _dpreKIOcmd    :: Maybe CmdSpec
  , _dpostKIOcmd   :: Maybe CmdSpec
  } deriving (Generic, D.FromDhall, Show)
makeLenses ''DhallCfg


-- loadDhallCfg :: MonadIO m => FilePath -> m DhallCfg
-- loadDhallCfg f = do
--   let opt = D.defaultInterpretOptions { D.fieldModifier = T.drop 1 }
--   let dec = D.genericAutoWith opt
--   liftIO $ D.inputFile dec f


-- testDhall :: IO ()
-- testDhall = pPrint =<< loadDhallCfg "/home/david/prj/kmonad/cfg/linux.dhall"
