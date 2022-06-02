-- NOTE: we will rename this module to Types when ready
module KMonad.App.TypesNew where

import KMonad.Prelude

import System.FilePath.Glob (glob)
import UnliftIO.Directory (getXdgDirectory, XdgDirectory(XdgConfig), )
import RIO.FilePath ((</>))

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

data File = FullPath FilePath | XdgCfgFile Text | Glob Text
  deriving (Eq, Show)

data FileError
  = GlobNoMatch         Text
  | GlobMultipleMatches Text [FilePath]
  deriving Eq

instance Show FileError where
  show (GlobNoMatch t) =
    "Glob \"" <> unpack t <> "\" matched 0 files"
  show (GlobMultipleMatches t fs)  = unpack . unlines $ [
    "Glob \"" <> t <> "\" matched multiple files:"
    ] <> map pack fs
instance Exception FileError

-- | Turn a conceptual file location into an actual FilePath
getPath :: MonadIO m => File -> m FilePath
getPath (FullPath f) = pure f
getPath (Glob t) = liftIO (glob $ unpack t) >>= \case
  [] -> throwIO $ GlobNoMatch t
  [f] -> pure f
  fs  -> throwIO $ GlobMultipleMatches t fs
getPath (XdgCfgFile t) = getXdgDirectory XdgConfig ("kmonad" </> unpack t)
